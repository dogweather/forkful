---
date: 2024-02-03 19:03:22.617664-07:00
description: "Writing tests in Fish Shell involves creating scripts that automatically\
  \ run your code to validate its behavior against expected outcomes. This practice\u2026"
lastmod: '2024-03-13T22:45:00.477425-06:00'
model: gpt-4-0125-preview
summary: "Writing tests in Fish Shell involves creating scripts that automatically\
  \ run your code to validate its behavior against expected outcomes. This practice\u2026"
title: Writing tests
---

{{< edit_this_page >}}

## What & Why?

Writing tests in Fish Shell involves creating scripts that automatically run your code to validate its behavior against expected outcomes. This practice is crucial as it ensures your shell scripts work as intended, catching errors early and making maintenance easier.

## How to:

Fish doesn't have a built-in testing framework like some other programming environments. However, you can write simple test scripts that use assertions to check the behavior of your functions. Additionally, you can leverage third-party tools like `fishtape` for a more comprehensive testing suite.

### Example 1: Basic Test Script

Let's start with a basic function in Fish that calculates the sum of two numbers:

```fish
function add --description 'Add two numbers'
    set -l sum (math $argv[1] + $argv[2])
    echo $sum
end
```

You can write a basic test script for this function like so:

```fish
function test_add
    set -l result (add 3 4)
    if test $result -eq 7
        echo "test_add passed"
    else
        echo "test_add failed"
    end
end

test_add
```

Running this script would output:

```
test_add passed
```

### Example 2: Using Fishtape

For a more robust testing solution, you can use `fishtape`, a TAP-producing test runner for Fish.

First, install `fishtape` if you haven't already:

```fish
fisher install jorgebucaran/fishtape
```

Next, create a test file for your `add` function, e.g., `add_test.fish`:

```fish
test "Adding 3 and 4 yields 7"
    set result (add 3 4)
    echo "$result" | fishtape
end
```

To run the test, use the following command:

```fish
fishtape add_test.fish
```

Sample output might look like:

```
TAP version 13
# Adding 3 and 4 yields 7
ok 1 - test_add passed
```

This tells you that the test passed successfully. `fishtape` allows you to structure more detailed tests and provides informative output, facilitating easier debugging and comprehensive test coverage for your Fish scripts.
