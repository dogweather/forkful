---
title:                "Logging"
date:                  2024-01-25T02:04:02.274630-07:00
model:                 gpt-4-1106-preview
simple_title:         "Logging"

category:             "Ruby"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/logging.md"
---

{{< edit_this_page >}}

## What & Why? 
Logging in programming is like keeping a diary for your application. It's the systematic recording of events, messages, and data points that give you insight into what your application is doing and how it's behaving. Coders log because it's crucial for debugging, monitoring application health, and getting clues about potential issues before they blow up into real problems.

## How to:
Ruby comes with a built-in module for logging, `Logger`, that’s super easy to use. Here's a quick example to get you started:

```ruby
require 'logger'

# Create a Logger that outputs to STDOUT
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# Example log messages
logger.info("This is an info message")
logger.warn("This is a warning message")
logger.error("This is an error message")
```

Running the above script will output something like this:

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : This is an info message
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : This is a warning message
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : This is an error message
```

You can configure the log format and level to filter out unnecessary noise, and you can direct logs to different outputs, like a file or even an external logging service.

## Deep Dive
Logging is like an age-old tradition in programming. Historically, logs were simple text files, manually parsed with tools like `grep`. But the concept spiraled into an entire ecosystem of robust logging frameworks and services like Log4j, Syslog on Linux, or Sematext and Loggly in the cloud era.

Ruby's `Logger` is a no-frills way to get started, but if you need more horsepower and flexibility, you might check out alternatives like Lograge or Semantic Logger. These libraries play well with Ruby applications, offering more granular control over log formatting, including structured logs (JSON format), better performance, and seamless integration with other services.

Each Ruby logging library has its own way of doing things, but under the hood, they all revolve around the idea of a logger instance that you send messages to. The logger handles these messages based on set levels—DEBUG, INFO, WARN, ERROR, FATAL, and UNKNOWN—and decides what to do with them: print them out, save them to a file, send them over the network, etc.

## See Also
For a deep dive into Ruby's built-in logging module, check out the official docs:

If you're interested in more advanced logging or want to explore third-party gems:
- [Lograge](https://github.com/roidrage/lograge)

For general logging practices and philosophy (not Ruby-specific), these articles are timeless reads:
- [Google's Site Reliability Engineering Book - Chapter 16: Handling Overload](https://sre.google/sre-book/handling-overload/#log-messages)
- [The 12 Factor App - Logs](https://12factor.net/logs)
