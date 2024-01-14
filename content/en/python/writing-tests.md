---
title:    "Python recipe: Writing tests"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why

Writing tests is an essential practice for any software developer, as it ensures that the code we write is functioning correctly and free of bugs. Tests also serve as a safety net when making changes to code, as they quickly catch any issues that may arise.

## How To

To write tests in Python, we will be using the built-in `unittest` module. Let's start by creating a simple function that adds two numbers together and returns the result.

```python
def add(x, y):
    return x + y
```

Next, we will create a test case using the `unittest` package. This will involve creating a class that inherits from `unittest.TestCase` and writing test methods that start with `test_`. These methods will contain the code to assert the expected output of our function against the actual output.

```python
import unittest

class TestAddFunction(unittest.TestCase):

    def test_add(self):
        result = add(2, 3)
        self.assertEqual(result, 5) # assert expected output is equal to actual output

if __name__ == '__main__':
    unittest.main()
```

When we run this file, we should see the following output:

```bash
----------------------------------------------------------------------
Ran 1 test in 0.001s

OK
```

This tells us that our test passed successfully. However, if we were to change the expected output to a different value, we would see the following:

```bash
F
======================================================================
FAIL: test_add (__main__.TestAddFunction)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "test.py", line 8, in test_add
    self.assertEqual(result, 6)
AssertionError: 5 != 6

----------------------------------------------------------------------
Ran 1 test in 0.001s

FAILED (failures=1)
```

This lets us know that our test failed, and we need to check our code for any mistakes.

## Deep Dive

Apart from just testing the output of our functions, we can also use the `unittest` module for more in-depth testing. This includes testing for exceptions, edge cases, and even checking if a specific line of code was executed. `unittest` also provides various assert methods such as `assertRaises`, `assertIn`, and `assertNotEqual` for more specific assertions.

Moreover, there are other testing frameworks for Python, such as PyTest and Doctest, that offer additional features and different testing styles. It is essential to explore and choose the best framework for your specific needs.

## See Also

- [Python `unittest` documentation](https://docs.python.org/3/library/unittest.html)
- [PyTest documentation](https://docs.pytest.org/en/latest/)
- [Doctest documentation](https://docs.python.org/3/library/doctest.html)