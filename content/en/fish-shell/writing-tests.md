---
title:                "Writing tests"
html_title:           "Fish Shell recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests is a way for programmers to ensure that their code is functioning correctly. It involves creating small, specific code snippets that check for expected results. Programmers do this in order to catch any potential errors or bugs before they become bigger problems.

## How to:

Writing tests in Fish Shell is easy and straightforward. Just follow these steps:

1. Start by creating a new .fish file for your tests.
2. Use the `test` command to create a test case.
3. Inside the test case, use the `begin` and `end` commands to specify the code you want to test.
4. Use the `assert` command to define the expected result.
5. Run the tests using the `-o` flag to see all the successful tests, or the `-e` flag to see all the failed tests.

Here's an example of how your test file might look like:

```Fish Shell
# This is a test file for myFunction
# Test case 1
test "myFunction Test 1"
    begin
    myFunction parameter
    end
    assert $? -eq 0 # This tests if the function returns 0, indicating success
# Test case 2
test "myFunction Test 2"
    begin
    myFunction wrongParameter
    end
    assert $? -ne 0 # This tests if the function returns a non-zero exit code, indicating failure
```

And here's an example of the output you might see:

```Fish Shell
> fish test_file.fish -o
myFunction Test 1 .. ok
myFunction Test 2 .. FAILED
1 test run, 1 failure
```

## Deep Dive:

Writing tests has become an important practice in modern software development. It is a form of automated testing, which means that tests are executed automatically without human intervention. This saves time and reduces the chances of human error.

An alternative to writing tests in Fish Shell is using a testing framework like `bats` or `shunit2`. However, using Fish Shell's built-in `test` command is often sufficient and does not require any additional dependencies.

Tests can also be used in conjunction with Continuous Integration (CI) tools, which run tests automatically whenever changes are made to the code. This helps catch any potential errors early on in the development process.

## See Also:

To learn more about writing tests in Fish Shell, check out the official documentation here: https://fishshell.com/docs/current/commands.html#test

For more information on automated testing and Continuous Integration, check out these resources:

- https://en.wikipedia.org/wiki/Automated_testing
- https://en.wikipedia.org/wiki/Continuous_integration