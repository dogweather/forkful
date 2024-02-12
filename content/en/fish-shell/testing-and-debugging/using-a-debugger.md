---
title:                "Using a debugger"
aliases:
- /en/fish-shell/using-a-debugger/
date:                  2024-01-25T20:50:19.217679-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using a debugger"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/using-a-debugger.md"
---

{{< edit_this_page >}}

## What & Why?
Using a debugger is all about squashing bugs—the nasty, time-sucking errors in your code. Programmers debug because they want to find and fix issues efficiently, understand code flow, and get a clearer picture of what their code is really doing.

## How to:
Fish doesn't have a built-in debugger like some other shells, but you can use external tools like `gdb` for debugging compiled programs or `fish -d` for running fish with debug output at different levels. Let's roll with `fish -d`:

```fish
# Run fish shell with debug level 2
fish -d2

# In the fish shell, let's test a simple function with a potential bug
function test_func
    set val 42
    echo "The value is $val"
    if test $val -eq 42
        echo "All is well."
    else
        echo "Something's fishy."
    end
end

# Call the function and observe the debug output
test_func
```

You'd see extra debug output before and after the function executes, helping you pinpoint issues.

## Deep Dive
Historically, debugging in Unix-like environments has been a province of specialized tools like `gdb` for C/C++ or `pdb` for Python. In Fish, you're usually reliant on external utilities or built-in features like `functions -v` for verbose output of functions and `set -x` to track variable changes.

Some folks choose alternative shells like Bash because of features like `set -x` for debugging scripts. However, Fish has its charm with a focus on user-friendliness and interactivity, which can reduce the need for hardcore debugging in many cases.

When it comes to implementation, debugging a script often involves running it with verbose output and tracing down where variables get set, unset, or mutated in unexpected ways. With Fish's color-coded output and user-friendly approach, you can often avoid the nitty-gritty of debugging – but when you're stuck, remember that verbosity and clarity are your best tools.

## See Also
Here are some trusty lifelines for when you're up to your fins in code:

- Fish documentation on debugging: https://fishshell.com/docs/current/index.html#debugging
- GDB (GNU Debugger) official guide: https://www.gnu.org/software/gdb/documentation/
- Stack Overflow Fish tag - real-world debugging cases: https://stackoverflow.com/questions/tagged/fish
- Advanced Bash-Scripting Guide - for comparing debugging approaches: https://tldp.org/LDP/abs/html/debugging.html
