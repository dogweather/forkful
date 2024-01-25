---
title:                "Logging"
date:                  2024-01-25T02:03:15.229184-07:00
model:                 gpt-4-1106-preview
simple_title:         "Logging"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/logging.md"
---

{{< edit_this_page >}}

## What & Why?
Logging is essentially jotting down what your program is doing, typically by writing out messages to a file or terminal. Programmers do it to keep track of events, diagnose problems, and to have an audit trail that tells the story of an application’s operation over time.

## How to:
Let's start with some basics. C doesn't have a built-in logging framework, but you can roll something simple with `stdio.h`. Here's how:

```c
#include <stdio.h>
#include <time.h>

void logMessage(const char* message) {
    time_t now;
    time(&now);
    char *date = ctime(&now);
    date[strlen(date) - 1] = '\0'; // Remove the newline at the end of ctime()'s result
    printf("[%s] %s\n", date, message);
}

int main() {
    logMessage("Application has started.");
    // ... your code goes here ...
    logMessage("Application is doing something important.");
    // ... your code continues ...
    logMessage("Application has ended.");
    return 0;
}
```

Sample output might look like this:

```
[Tue Mar 9 12:00:01 2023] Application has started.
[Tue Mar 9 12:00:02 2023] Application is doing something important.
[Tue Mar 9 12:00:03 2023] Application has ended.
```

Of course, in the real world you'd probably want to write to a file instead of the terminal, handle different log levels, and maybe use a predefined library.

## Deep Dive
Logging in C has a quaint charm—it's as low-level as most of the rest of the language. Historically, logging was performed using `fprintf` with `stderr` or a file pointer. As programs grew more complex, so did logging needs, leading to the development of libraries such as `syslog` on Unix systems, which could handle logging from multiple sources with various levels of importance.

In the modern landscape, there are plenty of C logging libraries out there, such as `zlog`, `log4c`, and `glog`, that offer a rich feature set including log rotation, structured logging, and multithreaded logging. These solutions allow for fine-grained control over log verbosity, destinations, and formats.

When implementing a logging system, details such as timestamp formatting, log file management, and performance need consideration. Timestamping logs is crucial for correlating events, while log rotation ensures that log files don't consume too much disk space. The act of logging should also be fast and non-blocking to the main application flow to prevent logging from becoming a bottleneck.

## See Also
To dive deeper into logging libraries and practices in C, check out these resources:

- GNU `syslog` manual: https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- `zlog`: A highly configurable logging library for C - https://github.com/HardySimpson/zlog
- `log4c`: A logging framework for C modeled after Log4j - http://log4c.sourceforge.net/
- `glog`: Google's application-level logging library - https://github.com/google/glog