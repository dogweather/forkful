---
title:                "Writing tests"
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests is creating little checks to ensure your code behaves as expected. Programmers write tests to catch bugs early, save time, and keep code reliable as it changes.

## How to:
Fish doesn't have a built-in testing framework, but you can use `fisher` to install one like `Fishtape`. Here's a simple test with `Fishtape`:

```fish
# Install Fishtape first
fisher install jorgebucaran/fishtape

# Create a test file, `test_my_function.fish`
function test_my_function
    echo "Running my_function tests"

    # Test case
    my_function argument
    echo $status | fishtape
end

# Run your test file in Fish Shell
fishtape test_my_function.fish
```

Sample output might look like this:

```
TAP version 13
ok 1 my_function with argument

1..1
# tests 1
# pass  1

# ok
```

## Deep Dive
Fish shell came about in 2005, way after Bash. It's been about smart features and user-friendliness from the start. Unlike Bash, it doesn't come piled with testing tools. That's where third-party tools like `Fishtape` come in, adding the missing test functionality to Fish. Remember, Fish scripts can be tested like any other script—by checking output and exit statuses—but with `Fishtape`, you get TAP-compliant output that's easier to use in CI/CD pipelines and with test harnesses.

## See Also
Check out these resources to dive deeper into Fish Shell and `Fishtape`:
- [Official Fish Documentation](https://fishshell.com/docs/current/index.html)
- [Fishtape on GitHub](https://github.com/jorgebucaran/fishtape)
- [Fisher Plugin Manager](https://github.com/jorgebucaran/fisher)
