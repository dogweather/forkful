---
title:                "Using a debugger"
date:                  2024-01-25T20:50:12.739110-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using a debugger"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/using-a-debugger.md"
---

{{< edit_this_page >}}

## What & Why?
Using a debugger is basically you playing detective in your code, sleuthing for bugs and figuring out why things aren't running smooth. Programmers do it because, let's face it, bugs are inevitable, and squashing them efficiently means getting your code up and running faster and more reliably.

## How to:
Gleam currently leans on the Erlang ecosystem for tooling, so you'll typically debug with tools like `rebar3`, `observer`, and `debugger`. Here’s how to get down and dirty with debugging:

```gleam
// In your rebar config, ensure you have these lines to include debug info:
{erl_opts, [debug_info]}.

// Run an Erlang shell with your app loaded
rebar3 shell

// Inside the shell, you can start the debugger
1> debugger:start().
```

Simple, right? The `debugger` GUI pops up, and you can set breakpoints, step through code, and watch variables to your heart's content. You won't see Gleam code directly, but the Erlang code it compiles to, which is still pretty helpful.

## Deep Dive
Gleam is a young language, so while it stands on the shoulders of the Erlang ecosystem, native Gleam debugging tools aren't yet in the spotlight. That means we're using Erlang's tried-and-true tools, and that's not a bad thing. Erlang's debugger has been around since the '90s, honed by years of eradicating pesky bugs in systems where reliability is key.

As for alternatives, tracing is a powerful method in the BEAM world (that's the virtual machine running Erlang and Elixir code). Using `rebar3` you can tap into tools like `recon` to trace function calls and dive deep into performance issues.

The switch between writing Gleam and debugging in Erlang might feel like you’re translating your thoughts on the fly. But the upside is you get a peek into the Erlang world, understanding the building blocks of your app in its runtime form.

## See Also
To expand your debugging toolkit, check out:

- Erlang's debugger documentation: [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- The `recon` library for Erlang: [https://ferd.github.io/recon/](https://ferd.github.io/recon/)
- On tracing in the BEAM: [https://adoptingerlang.org/docs/development/tracing/](https://adoptingerlang.org/docs/development/tracing/)