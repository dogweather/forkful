---
title:                "Logging"
date:                  2024-01-25T02:03:32.355707-07:00
model:                 gpt-4-1106-preview
simple_title:         "Logging"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/logging.md"
---

{{< edit_this_page >}}

## What & Why?
Logging is essentially how we record what goes on in our programs. It's like having a little black box; when stuff goes wrong (and trust me, it will), logs are invaluable for figuring out what happened, diagnosing issues, and optimizing performance.

## How to:
In Gleam, you'd typically pull in a logging library—there isn't a dedicated logging mechanism out of the box. Let's say we're using a hypothetical `gleam_logger` crate. Here's how you could integrate it:

```gleam
import gleam_logger

pub fn main() {
  gleam_logger.info("App is starting up!")
  let result = intense_calculation()

  case result {
    Ok(value) -> 
      gleam_logger.debug("Calculation successful", value)
    Error(err) -> 
      gleam_logger.error("Calculation failed", err)
  }
}
```

Expected output in your logs would look something like this:

```
INFO: App is starting up!
DEBUG: Calculation successful 42
ERROR: Calculation failed Reason: Divide by zero
```

## Deep Dive
The art of logging has been around since the early days of programming. System operators would literally get logs from the computer - making sure everything ran smoothly. Fast forward, and logging has gone digital, becoming a core part of software development.

While Gleam, being a relatively young language that targets the Erlang ecosystem, doesn't have a built-in logging framework, you can leverage the mature Erlang logging facilities or other community-provided libraries. Each has different features and trade-offs: some might provide structured logging, others are more for simple text output.

Now, the question of implementing a logging facility: Is it simple? At a glance, yes. But peel back the layers, and you're looking at handling concurrency, I/O bottlenecks, log rotation, format standardization (think JSON for structured logging), level filtering, and possibly distributed tracing. Plus, in a functional paradigm, you generally want side-effects (like logging) handled in a predictable and controlled manner.

## See Also
Here's where you can find more about the nuts and bolts of logging in Gleam and its surrounding ecosystem:
- [Erlang's :logger documentation](http://erlang.org/doc/apps/kernel/logger_chapter.html): Since Gleam compiles to Erlang, this is directly applicable.
- [Gleam's standard library docs](https://hexdocs.pm/gleam_stdlib/): For updates on any logging utilities that might get added.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam): A curated list of resources, which might include logging libraries as they become available.
