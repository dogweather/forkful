---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

# Article: Debug Output in Fish Shell

## What & Why?

Printing debug output is a programmer's tactical move to identify and fix bugs during code runtime. It's like leaving breadcrumbs along the path to trace back the problem spots.

## How to:

In Fish Shell, we can use the 'echo' command. Below is a simple example:

```Fish Shell
function greet
  echo "Hello, World!"
end

greet
```

The console will print:

```Fish Shell
Hello, World!
```

Now, let's level up with debugging info:

```Fish Shell
function debug_greet
  echo "DEBUG: Entering debug_greet function"
  echo "Hello, Debugging World!"
  echo "DEBUG: Exiting debug_greet function"
end

debug_greet
```

And poof! Here's your debug output:

```Fish Shell
DEBUG: Entering debug_greet function
Hello, Debugging World!
DEBUG: Exiting debug_greet function
```

## Deep Dive

Now, while the 'echo' method is efficient, it comes with a pinch of history. Introduced in Version 2.3.0, Fish incorporates 'printf' redirection commands for more advanced debug output tactics.

Consider alternatives like 'stderr' for error reporting or use Fish's built-in 'fish_trace' for function call tracebacks without altering code.

As a matter of implementation, Fish handles debug output at shell level, not within the language or libraries. That heightens efficiency and lets the core language stay clean.

## See Also:

1. [Fish Official Documentation](https://fishshell.com/docs/current/index.html) for an extensive guide to Fish Shell programming.
2. [Fish GitHub Repository](https://github.com/fish-shell/fish-shell) to catch up with ongoing development.
3. [Fish Shell Tutorial by DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-use-the-fish-shell) for a comprehensive, user-friendly start. 

Remember, mastering debug is what separates good coders from rushed ones. So, off you go! Happy Fishing!