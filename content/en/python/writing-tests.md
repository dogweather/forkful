---
title:                "Writing tests"
html_title:           "Python recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Tests in programming are code snippets that check if your main code is functioning properly. They're essential because they help coders catch errors early, saving time and money in the long run.

## How to:
Let's take the example of a simple Python function that a code tests.

Function:
```Python
def add_numbers(a, b):
    return a + b
```
And its test:
```Python
def test_add_numbers():
    assert add_numbers(2, 3) == 5, "Should be 5"
    assert add_numbers(-1, 1) == 0, "Should be 0"
```
Running the script:
```Python
test_add_numbers() 
```
If there's no output, it means your test passed. If there's an error with the function `add_numbers()`, you'll get an error message like `AssertionError: Should be 5`.

## Deep Dive
Historically, testing software was an afterthought. Bayerin Marick's "The rise and fall of the Chaos model", depicts how the community shifted towards test-driven development around the 1990s. 

Now, there are various tools and types, like unit tests, integration tests, acceptance tests, etc., each serving a specific purpose. Alternatives to writing tests yourself include using static typing and formal verification although these methods are not foolproof.

Python's `unittest` library is commonly used for writing tests, but PyTest is simpler and more Pythonic. Implementation varies depending on the system, though the basics are often running the code under different scenarios and checking the output.

## See Also
- "Test Driven Development: By Example" by Kent Beck - seminal work that championed the idea of writing tests first.
- PyTest Documentation - provides several examples and best practices.
- "The Art of Unit Testing" by Roy Osherove - presents the basics of unit testing.

Try to always keep your codebase written with tests. Happy coding!