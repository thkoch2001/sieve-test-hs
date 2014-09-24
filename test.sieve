# from https://github.com/Exim/exim/wiki/MailFilteringTips
require [ "regex", "variables", "fileinto", "envelope" ];

# split out the various list forms
# Mailman & other lists using list-id
if exists "list-id" {
        if header :regex "list-id" "<([a-z0-9-]+)[.@]" {
                set :lower "listname" "${1}";
                fileinto "list.${listname}";
        } else {
            if header :regex "list-id" "^\\s*<?([a-z0-9-]+)[.@]" {
                set :lower "listname" "${1}";
                fileinto "list.${listname}";
            } else {
                fileinto "list.unknown";
            }
        }
        stop;}
# Listar and mailman like
elsif exists "x-list-id" {
        if header :regex "x-list-id" "<([a-z0-9-]+)\\\\." {
                set :lower "listname" "${1}";
                fileinto "list.${listname}";
        } else {
                fileinto "list.unknown";
        }
        stop;}
# Ezmlm
elsif exists "mailing-list" {
        if header :regex "mailing-list" "([a-z0-9-]+)@" {
                set :lower "listname" "${1}";
                fileinto "list.${listname}";
        } else {
                fileinto "list.unknown";
        }
        stop;}
# York lists service
elsif exists "x-mailing-list" {
        if header :regex "x-mailing-list" "^\\s*([a-z0-9-]+)@?" {
                set :lower "listname" "${1}";
                fileinto "list.${listname}";
        } else {
                fileinto "list.unknown";
        }
        stop;}
# Smartlist
elsif exists "x-loop" {
        # I don't have any of these to compare against now
        fileinto "list.unknown";
        stop;}
# don't know what mailer
elsif exists "X-listname" {
        if header :regex "X-listname" "<([a-z0-9-]+)@" {
                set :lower "listname" "${1}";
                fileinto "list.${listname}";
        } else {
                fileinto "list.unknown";
        }
        stop;}
# poorly identified
elsif envelope :contains "from" "owner-" {
        if envelope :regex "from" "owner-([a-z0-9-]+)-outgoing@" {
                set :lower "listname" "${1}";
                fileinto "list.${listname}";
        } elsif envelope :regex "from" "owner-([a-z0-9-]+)@" {
                set :lower "listname" "${1}";
                fileinto "list.${listname}";
        } elsif header :regex "Sender" "owner-([a-z0-9-]+)@" {
                set :lower "listname" "${1}";
                fileinto "list.${listname}";
        } else {
                fileinto "list.unknown";
        }
        stop;}
# other poorly identified
elsif  envelope :contains "from" "-request" {
        if envelope :regex "from" "([a-z0-9-]+)-request@" {
                set :lower "listname" "${1}";
                fileinto "list.${listname}";
        } else {
                fileinto "list.unknown";
        }
        stop;}
