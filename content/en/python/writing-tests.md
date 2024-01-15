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

## Why

Writing tests may seem like an extra step in the coding process, but it ultimately saves time and effort in the long run. By testing your code, you can catch any errors or bugs early on and ensure that your code is functioning as intended.

## How To

Writing tests in Python is made easy with the built-in `unittest` module. Let's say we have a simple function that adds two numbers together:

```Python
def add(x, y):
    return x + y
```

To write a test for this function, we can create a new file and import the `unittest` module. Then, we can define a new test case and add a test method to it using the `assertEqual()` function to verify that our function is returning the correct output:

```Python
import unittest

class TestAdd(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(5, 10), 15)
```

We can run this test by simply executing our test file, and if all goes well, we should see that our test passes!

Now let's say we want to add some more functionality to our `add()` function, such as checking for invalid inputs. We can easily add this test by using the `assertRaises()` function:

```Python
def add(x, y):
    if not isinstance(x, int) or not isinstance(y, int):
        raise TypeError("Arguments must be integers")
    return x + y
```

```Python
import unittest

class TestAdd(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(5, 10), 15)
    def test_invalid_input(self):
        self.assertRaises(TypeError, add, "5", 10)
```

Now, if we run our tests again, we should see that the second test fails, since we are passing in a string instead of an integer.

## Deep Dive

Writing tests not only helps ensure that your code is functioning correctly, but it also promotes good coding practices. By writing tests, you are essentially breaking down your code into smaller, more manageable chunks and checking each one individually. This not only makes debugging easier, but it also allows for easier testing of new features and catching potential bugs.

Another useful tool for testing in Python is the `pytest` module, which provides additional features such as support for parametrized testing and fixtures. It also has a more user-friendly and readable syntax compared to the `unittest` module.

It's important to remember that writing tests should not be an afterthought, but rather an integral part of the coding process. By writing tests along with your code, you are ensuring that your code is reliable and maintainable.

## See Also

- [Python unittest documentation](https://docs.python.org/3/library/unittest.html)
- [pytest documentation](https://docs.pytest.org/en/latest/)