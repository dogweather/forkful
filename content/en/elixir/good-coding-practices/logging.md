---
title:                "Logging"
aliases:
- /en/elixir/logging/
date:                  2024-01-25T02:03:45.600637-07:00
model:                 gpt-4-1106-preview
simple_title:         "Logging"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/logging.md"
---

{{< edit_this_page >}}

## What & Why?
Logging in software development is the technique of recording events that occur while a program is running, typically to a file or external system. Programmers do it to gain insights into the software's behavior, troubleshoot issues, and maintain a record of operational history which is crucial for debugging and monitoring the health of applications.

## How to:
In Elixir, the primary way to log information is through the built-in `Logger` module. Here's how you can use it:

```elixir
defmodule MyApplication do
  require Logger

  def do_something_important(param) do
    Logger.info("Starting important process with param: #{param}")

    # Simulate work being done
    :timer.sleep(1000)

    Logger.debug("Process completed.")
  rescue
    error -> Logger.error("An error occurred: #{inspect(error)}")
  end
end

# To see your logs, you just call the function:
MyApplication.do_something_important("MyParam")
```

This simple snippet shows how to log at different levels (`info`, `debug`, and `error`). When you run this, you won't see the debug message unless you configure the Logger level to `:debug`. By default, Elixir's Logger filters out log messages below `:info`.

Sample output at the `:info` level might look like this:
```
14:32:40.123 [info]  Starting important process with param: MyParam
14:32:41.126 [error] An error occurred: %RuntimeError{message: "runtime error"}
```

## Deep Dive:
Elixir's `Logger` is a built-in tool that's been part of the language since its early days. It is influenced by the logging systems from other BEAM languages like Erlang. The logger provides different levels of logging – `:debug`, `:info`, `:warn`, and `:error` – and it’s pluggable, allowing different backends to be hooked in for handling log messages.

One alternative to the built-in Logger for more complex scenarios is the use of logging libraries such as `Logstash` or `Sentry` for Elixir, which can provide additional features like error tracking and aggregation in a more visual format. For local development, Elixir developers often rely on built-in Logger functionality for its simplicity and integration with the BEAM VM.

Under the hood, the Logger module offers asynchronous and synchronous logging. Asynchronous logging, which is the default, does not block the execution of your application while logging the messages. This ensures that logging does not negatively affect performance. However, synchronous logging can be enabled for cases where you need to guarantee that messages are logged in the order they were sent.

The Logger configuration can be adjusted in the `config/config.exs` file of an Elixir application, where you can set the logging level, format, metadata, and more. Always remember to adjust your logging levels and outputs for different environments; you wouldn't want verbose debug logs flooding your production systems.

## See Also:
- The official Elixir Logger documentation: https://hexdocs.pm/logger/Logger.html
- A blog post on Elixir logging best practices: https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Sentry for Elixir on Hex: https://hex.pm/packages/sentry
- Elixir School's lesson on Logger: https://elixirschool.com/en/lessons/specifics/debugging/#logging
