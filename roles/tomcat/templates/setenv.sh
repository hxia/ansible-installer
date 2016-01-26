#!/bin/sh
# Add memory settings to CATALINA_OPTS
export JAVA_HOME={{ java_dest }}/{{ java_version }}

# Add memory settings to CATALINA_OPTS
export CATALINA_OPTS="-server -Xms4096m -Xmx8192m -XX:MaxPermSize=512m -XX:+UseParallelGC "

