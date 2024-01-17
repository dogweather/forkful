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

Writing tests in Python is essentially creating automated scripts to check the functionality of your code. It allows developers to catch bugs and ensure the accuracy of their code, ultimately resulting in more reliable software.

## How to:

Writing tests in Python is as simple as creating functions that test specific aspects of your code. Let's take a look at an example:

```
def test_addition():
    assert 2+2 == 4
```
The above function is testing if the result of 2+2 equals 4. If you run this test and see no output, that means the test passed. However, if you were to change the assert statement to something like `assert 2+2 == 5`, the test would fail and you would see an output indicating that the test failed.

To run this test, simply type `pytest` in your terminal while in the directory containing your tests. You can also specify which test file to run by using `pytest <test_file_name>`.

## Deep Dive:

Writing tests has been around for a long time and is a fundamental part of software development. It allows for a more structured approach to writing code and helps catch errors that might otherwise be missed during manual testing.

There are also alternatives to writing tests in Python, such as using external testing frameworks like Selenium or unit testing frameworks like unittest or nose.

When it comes to actually implementing tests, it's important to keep them organized and separate from your actual code. This allows for easier maintenance and prevents clutter in your codebase.

## See Also:

- [Python Testing Tutorial](https://realpython.com/python-testing/)
- [Pytest Documentation](https://docs.pytest.org/en/6.2.x/)
- [Unittest Documentation](https://docs.python.org/3/library/unittest.html)