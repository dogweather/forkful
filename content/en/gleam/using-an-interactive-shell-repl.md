---
title:                "Using an interactive shell (REPL)"
date:                  2024-01-25T03:39:50.326130-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using an interactive shell (REPL)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## What & Why?

A REPL, short for Read-Eval-Print Loop, is a programming tool for interactively running code and seeing results instantly. Programmers use it to experiment, debug, or learn a new language on the fly like Gleam.

## How to:

Gleam currently doesn't include a REPL within its standard distribution. However, you can experiment with Gleam code using the existing Erlang shell because Gleam compiles to Erlang bytecode. Here's how:

1. Compile your Gleam code to Erlang.
```plaintext
gleam build
```

2. Start the Erlang shell.
```plaintext
erl -pa ebin
```

3. Call your Gleam functions (assuming you have a module named `my_mod` and function `my_fun`).
```erlang
my_mod:my_fun().
```

You should see the output of your function displayed in the shell.

## Deep Dive

REPL embodies the dynamic and exploratory spirit of many functional programming languages, tracing back to LISP's REPL in the 1960s. Comparatively, other systems like Python's `ipython` or Ruby's `irb` offer similar experiences for their communities. 

While Gleam doesn't have a native REPL yet, leveraging the Erlang shell remains a nifty workaround. The Erlang shell's capabilities come from the BEAM VM, the virtual machine powering the Erlang ecosystem, which includes Elixir, LFE, and Gleam.

Alternatives to REPLs in the Gleam ecosystem could include writing test cases or using online compilers and code playgrounds that support Gleam, to test out snippets of code outside a full project setup.

The implementation of a dedicated Gleam REPL faces challenges mainly around the compiled nature of Gleam and Erlang's runtime, where hot code swapping is the norm. Any future Gleam REPL would need to reconcile the language's static typing with the dynamic execution environment a REPL expects.

## See Also

- Gleam's official documentation: https://gleam.run/book/
- Erlang's shell documentation: http://erlang.org/doc/man/erl.html
- An online Gleam compiler playground: https://gleam.run/compiler/