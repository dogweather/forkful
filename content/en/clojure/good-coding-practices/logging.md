---
date: 2024-01-25 02:04:08.781778-07:00
description: "Logging is essentially the software equivalent of a ship's log; it's\
  \ a way to record events that happen while an application is running. Programmers\
  \ do it\u2026"
lastmod: '2024-03-13T22:44:59.751456-06:00'
model: gpt-4-1106-preview
summary: Logging is essentially the software equivalent of a ship's log; it's a way
  to record events that happen while an application is running.
title: Logging
weight: 17
---

## How to:
Clojure leans on Java's logging facilities, but you can tap into them in a more idiomatic Clojure way. Let's take a look at how you might use `clojure.tools.logging`, which provides a simple abstraction over several logging frameworks:

First, add a dependency for `clojure.tools.logging` and a logging implementation such as `log4j` in your `project.clj`:

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

Now, let's log some messages:

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "Starting intense computation...")
  (Thread/sleep 3000) ; Simulating a long computation
  (log/info "Computation done. The answer is 42.")
  42)

(compute-answer-to-everything)
```
The output won't show `DEBUG` messages by default, as log levels are typically set to `INFO`:

```
INFO  [your-namespace] - Computation done. The answer is 42.
```

You can configure the log levels and appenders in a `log4j.properties` file to get more verbose output if needed.

## Deep Dive
Clojure's `clojure.tools.logging` has been around for a while and serves as a bridge between Clojure code and the Java logging world. Historically, Java has gone through several iterations and libraries for logging such as Java's built-in logging API, `log4j`, `slf4j`, and `logback`.

In Clojure, while you can directly use Java's logging frameworks, `clojure.tools.logging` detects and delegates to whichever logging framework it finds in your classpath, saving you from being tightly coupled to a specific implementation. This can help keep your Clojure code more portable and modular.

Alternatives to `clojure.tools.logging` within the Clojure ecosystem include libraries like `timbre`, which is a pure Clojure logging library with features like log rotation, filtering, and asynchronous logging out of the box.

Implementation details are crucial when it comes to logging in a multi-threaded environment such as Clojure. Here, immutability and side-effect management provide distinct advantages. Logging, as a side-effect, should be handled with care to avoid performance bottlenecks and ensure thread-safety, which most Java logging frameworks already take care of.

Lastly, consider structured logging, where logs are written as structured data (like JSON). This can be extremely useful for later analysis and processing, particularly when dealing with large-scale distributed systems.

## See Also
If you're hungry for more, consider checking out these resources:

- Clojure Tools Logging documentation: https://github.com/clojure/tools.logging
- Timbre, a Clojure logging library: https://github.com/ptaoussanis/timbre
- Configuring Log4J in Clojure: http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- Logback Manual for advanced setups: http://logback.qos.ch/manual/
- A guide on structured logging in Clojure: https://corfield.org/blog/2020/04/28/structured-logging/
