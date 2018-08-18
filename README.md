celtx

To be renamed 'drood'

Script Writing and Production Suite
=====

To build for Linux

On a clean build of Hardy Heron (Ubuntu 8.04 LTS, yes I know it's stupid old and EOL'd)

Update repositories to use the eol repos so we can install anything. Open /etc/apt/sources.list in your favourite text editor, and replace all occurrences of 'http://us.archive.ubuntu.com/ubuntu/ hardy' (it doesn't matter what comes before or after, that's the consistent chunk we're changing) with 'http://old-releases.ubuntu.com/ubuntu/ hardy'.

apt-get update

apt-get upgrade

apt-get install build-essential libasound2-dev libcurl4-openssl-dev libnotify-dev libxt-dev libiw-dev mesa-common-dev autoconf2.13 yasm libidl-dev

apt-get build-dep firefox

Now open up the mozilla folder, and run the 'make_all_linux.sh' script. It should do the rest. currently the only locale that is building is en-US because of some issue I haven't tracked down. 
