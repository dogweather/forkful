---
title:    "Fish Shell recipe: Writing tests"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why Writing Tests in Fish Shell Can Improve Your Programming Skills

As developers, we often find ourselves repeating the same tasks over and over again. Writing tests can help streamline this process by automating testing and reducing manual errors. In this blog post, we will explore the benefits of writing tests in Fish Shell and how it can improve our overall programming skills.

## How To Write Tests in Fish Shell

Writing tests in Fish Shell is a simple process. First, let's create a new fish function called `test_addition` which will test our addition function.

```Fish Shell
function test_addition
    # Test for addition function
    result=(addition 2 3)
    if [ "$result" -eq 5 ]
        echo "Test passed: addition(2, 3) equals 5"
    else
        echo "Test failed: addition(2, 3) does not equal 5"
    end
end
```

Next, we can call the `test_addition` function to run our test.

```Fish Shell
test_addition
```

The output of this test should be:

```
Test passed: addition(2, 3) equals 5
```

## Deep Dive into Writing Tests in Fish Shell

Now that we have a basic understanding of writing tests in Fish Shell, let's dive deeper into the process. Fish Shell provides various testing functions such as `assert` and `expect` which allow us to verify values and conditions. These functions can be combined with loops and conditional statements to create more complex and thorough tests.

It is also important to note that Fish Shell has a built-in testing framework called `bass` which makes writing tests even easier. With `bass`, we can separate our tests into different files, making it easier to organize and maintain our codebase.

Another benefit of writing tests in Fish Shell is that it allows us to practice writing concise and efficient code. By breaking down our code into smaller functions and writing tests for them, we can improve our understanding of the logic behind our code and optimize it for better performance.

## See Also

- Official Fish Shell documentation on testing: https://fishshell.com/docs/current/cmds/bass.html
- A beginner's guide to writing tests in Fish Shell: https://medium.com/dev-genius/automate-your-development-process-with-fish-shell-tests-cf668308d188
- A comparison of different testing frameworks in Fish Shell: https://blog.rocketeerie.com/posts/dev/testing-fish/