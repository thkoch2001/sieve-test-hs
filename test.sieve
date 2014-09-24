# from https://github.com/Exim/exim/wiki/MailFilteringTips
require [ "regex", "variables", "fileinto", "envelope", "mailbox" ];

# todo
# Smartlist
# elsif exists "x-loop" {
# I don't have any of these to compare against now

if exists "list-id" { # Mailman & other lists using list-id
  if header :regex "list-id" "<([a-z0-9-]+)[.@]" {
    set :lower "listname" "${1}";
  } elsif header :regex "list-id" "^\\s*<?([a-z0-9-]+)[.@]" {
    set :lower "listname" "${1}";
  }
} elsif exists "x-list-id" { # Listar and mailman like
  if header :regex "x-list-id" "<([a-z0-9-]+)\\\\." {
    set :lower "listname" "${1}";
  }
} elsif exists "mailing-list" { # Ezmlm
  if header :regex "mailing-list" "([a-z0-9-]+)@" {
    set :lower "listname" "${1}";
  }
} elsif exists "x-mailing-list" { # York lists service
  if header :regex "x-mailing-list" "^\\s*([a-z0-9-]+)@?" {
    set :lower "listname" "${1}";
  }
} elsif exists "X-listname" { # don't know what mailer
  if header :regex "X-listname" "<([a-z0-9-]+)@" {
    set :lower "listname" "${1}";
  }
} elsif envelope :contains "from" "owner-" { # poorly identified
  if envelope :regex "from" "owner-([a-z0-9-]+)-outgoing@" {
    set :lower "listname" "${1}";
  } elsif envelope :regex "from" "owner-([a-z0-9-]+)@" {
    set :lower "listname" "${1}";
  } elsif header :regex "Sender" "owner-([a-z0-9-]+)@" {
    set :lower "listname" "${1}";
  }
} elsif  envelope :contains "from" "-request" { # other poorly identified
  if envelope :regex "from" "([a-z0-9-]+)-request@" {
    set :lower "listname" "${1}";
  }
}

if not string :is "${listname}" "" {
  fileinto "list.${listname}";
}
