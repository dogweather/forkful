---
title:                "Python recipe: Writing tests"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/writing-tests.md"
---

{{< edit_this_page >}}

## Why

As a programmer, writing tests may seem like an additional task on top of writing code. However, investing time in writing tests can actually save time and effort in the long run. It ensures that your code works as expected and helps catch bugs early on, making it an essential part of the development process.

## How To

Writing tests in Python is fairly simple and can be done using the built-in `unittest` module. Let's take a look at an example of writing a test for a function that adds two numbers:

```python
# Import the `unittest` module
import unittest

# Define a function to be tested
def add_numbers(x, y):
    return x + y

# Create a class for testing our function
class TestAddNumbers(unittest.TestCase):
    # Create a test method
    def test_add_numbers(self):
        result = add_numbers(5, 10)
        # Assert if the result is equal to the expected output
        self.assertEqual(result, 15)

# Run the tests
unittest.main()
```

Output:
```
..
----------------------------------------------------------------------
Ran 2 tests in 0.000s

OK
```

In the code block above, we first import the `unittest` module. Then we define the function `add_numbers` that takes in two numbers and returns their sum. Next, we create a class `TestAddNumbers` that inherits from `unittest.TestCase`. This allows us to use built-in methods such as `assertEqual()` to check if the result of our function is equal to the expected output. Finally, we run our test using `unittest.main()`.

Writing tests for more complex functions follows a similar structure. You can also use different `assert` methods such as `assertTrue()` or `assertFalse()` depending on the purpose of your test.

## Deep Dive

When writing tests, it's important to keep in mind the various cases your code may encounter. This includes testing for different input values, edge cases, and even errors or exceptions. Writing tests for these cases can help ensure that your code is robust and can handle unexpected situations.

Another important practice is to write tests along with your code. This means that for every function or feature you add, you also write a test for it. This ensures that your tests are always up-to-date and reflects any changes made to your code.

It's also worth mentioning that there are other Python testing frameworks available such as `pytest` or `nose`. These offer additional features and a different syntax compared to `unittest`, so it's worth exploring and finding the one that works best for you and your team.

## See Also

- [Python's official unittest documentation](https://docs.python.org/3/library/unittest.html)
- [A detailed guide to writing tests in Python](https://realpython.com/python-testing/)
- [Comparing different Python testing frameworks](https://medium.com/nuances-of-programming/comparing-python-testing-frameworks-unittest-vs-pytest-eafd3928db7c)