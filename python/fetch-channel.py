#!/usr/bin/python
#
# Usage: fetch-channel URL DIRECTORY NAME
#
# $Id: fetch-channel.py,v 1.23 2008/06/18 17:48:08 xach Exp $

import feedparser, sys, os, types, calendar, hashlib
from time import time, gmtime
from glob import glob

UnicodeTranslations = {0x2018: "'",
                       0x00A0: "&nbsp;",
                       0x2019: "'",
                       0x201C: "\"",
                       0x201D: "\"",
                       0x2010: "-",
                       0x2013: "-",
                       0x2014: "&mdash;",
                       0x2022: "&middot;",
                       0x2026: "..."}

def file_exists(file):
    return os.access(file, os.F_OK)

def has_tag(required_tags, item_tags):
    """Does any string in 'required_tags' match any string in 'item_tags'?"""
    for rtag in required_tags:
        for itag in item_tags:
            if rtag == itag:
                return 1

    return 0

def load_required_tags(filename):
    try:
        fh = file(filename, 'r')
        return [line.rstrip() for line in fh]
    except IOError:
        return None

def load_failure_count():
    try:
        fh = file('failure-count', 'r')
        return int(fh.next().rstrip())
    except IOError, ValueError:
        return 0

def increment_failure_count():
    count = load_failure_count() + 1
    fh = file('failure-count', 'w')
    fh.write(str(count))
    fh.write('\n')
    return count

def clear_failure_count():
    if file_exists('failure-count'):
        os.unlink('failure-count')

def pathname_list(array):
    pathnames = []
    for string in array:
        pathnames.append('#p"%s"' % string)
    return '(' + ' '.join(pathnames) + ')'


def is_list(object):
    return type(object) == types.ListType

def is_number(object):
    return type(object) in (types.IntType, types.FloatType, types.LongType)

def is_string(object):
    return type(object) in types.StringTypes


def write_number_readably(fh, object):
    try:
        fh.write('%d' % object)
    except OverflowError:
        fh.write('%d' % long(object))


def out_of_range(charcode):
    return ((charcode > 255) or
            (charcode == 0xA0) or
            ((charcode < 20) and (charcode not in (0x9, 0xA, 0xD))))

def write_string_readably(fh, object):
    fh.write('"')

    for char in object:
        charcode = ord(char)
        if out_of_range(charcode):
            char = UnicodeTranslations.get(charcode, '&#%d;' % charcode)
        elif charcode > 128:
            char = chr(ord(char))
         
        if char in ('\\', '"'):
            fh.write('\\')

        fh.write(char)
 
    fh.write('"')


def write_list_readably(fh, object):
    fh.write(pathname_list(object))

def write_object_readably(fh, object):
    if is_number(object):
        write_number_readably(fh, object)
    elif is_string(object):
        write_string_readably(fh, object)
    elif is_list(object):
        write_list_readably(fh, object)
    else:
        raise NotImplementedError
    

def write_sexp(fh, *args):
    fh.write("(")
    fh.write(args[0])

    args = args[1:]

    while args:
        fh.write(" :")
        fh.write(args[0])
        fh.write(" ")
        write_object_readably(fh, args[1])
        args = args[2:]

    fh.write(")\n");

def unix_to_universal_time(unixtime):
    return int(unixtime) + 2208988800

def directory_empty_p(directory):
    if not len(os.listdir(directory)):
        return 1
    else:
        return 0




def fetch_channel(url, directory, name):
    os.chdir(directory)

    try:
        if not directory_empty_p('./items/'):
            st = os.stat("channel-info.sexp")
            modified = gmtime(st.st_mtime)
        else:
            modified = None
    except OSError:
        modified = None

    useragent = "fetch-channel.py/1.0 (xach@xach.com; for http://planet.lisp.org/)"
    
    data = feedparser.parse(url, agent=useragent, modified=modified)

    channelfile = "channel-info.sexp"
    channeltmpfile = "channel-info.sexp.tmp"

    required_tags = load_required_tags("required-tags")

    #
    # Check to see if the feed is unmodified, missing, etc.
    #
    
    feed_status = data.get('status', 999)
    
    if modified != None and feed_status not in (200, 304):
        failed = increment_failure_count()
        print '>> %s status %d (failure #%d)' % (url, feed_status, failed)

    if feed_status != 200:
        #print '>> %s status %d (%s)' % (url, feed_status, modified)
        #os.utime(channelfile, None)
        return

    clear_failure_count()

    #
    # Get a table of existing files
    #

    old_files = {}

    for filename in glob('items/*.sexp'):
        old_files[filename] = 1


    #
    # Write new items
    #

    new_channel_p = directory_empty_p('./items')

    feed_files = []

    for item in data['items']:
        if required_tags:
            item_tags = [tag.get('term') for tag in item.get('tags', [])]
            if not has_tag(required_tags, item_tags):
                continue

        id = item.get('id', item.get('link', ''))
        digest = hashlib.md5(id)
        short_filename = '%s.sexp' % digest.hexdigest()
        feed_files.append(short_filename)
        filename = './items/' + short_filename
        tmpfilename = filename + '.tmp'

        date_value = item.get('issued_parsed',
                              item.get('date_parsed',
                                       item.get('modified_parsed',
                                                item.get('updated_parsed', 0))))

        if date_value:
            unixtime = int(calendar.timegm(date_value))
        else:
            if new_channel_p:
                unixtime = 0
            elif os.access(filename, os.F_OK):
                st = os.stat(filename)
                unixtime = st.st_mtime
            else:
                unixtime = time()

        fh = file(tmpfilename, "w")

        # for sexp's feed, maybe others, where the description is just a
        # summary and a "content" element contains the full stuff
        content = item.get('content', [{}])[0]
        description = content.get('value',
                                  item.get('description',
                                           item.get('summary', '')))
        author_detail = item.get('author_detail', {})
        author_name = author_detail.get('name', '');

        write_sexp(fh, "item",
                   "id", id,
                   "title", item.get('title', ''),
                   "author-name", author_name,
                   "description", description,
                   "date", unix_to_universal_time(unixtime),
                   "link", item['link'])

        fh.close()

        os.rename(tmpfilename, filename)

        os.utime(filename, (unixtime, unixtime))

        if filename in old_files:
            del(old_files[filename])

    #
    # Remove old files
    #

    #for old_file in old_files.keys():
    #os.unlink(old_file)

    cfh = file(channeltmpfile, "w")
    channel = data['channel']

    write_sexp(cfh, 'channel',
               'name', name,
               'title', channel.get('title', ''),
               'description', channel.get('description', ''),
               'url', channel.get('link', ''),
               'feed-url', url,
               'source', 'python',
               'current-item-files', feed_files,
               'last-fetch-time', unix_to_universal_time(time()))
               

    cfh.close()
    os.rename(channeltmpfile, channelfile)

if __name__ == '__main__':
    if len(sys.argv[1:]) != 3:
        print "Usage: fetch-channel URL DIRECTORY NAME"
        sys.exit(1)

    (url, directory, name) = sys.argv[1:]
    
    fetch_channel(url, directory, name)
