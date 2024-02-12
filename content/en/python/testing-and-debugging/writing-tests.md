---
title:                "Writing tests"
aliases:
- /en/python/writing-tests.md
date:                  2024-02-03T19:03:27.970246-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests in Python involves creating automated scripts to validate the correctness of your code. Programmers do this to ensure that their functions or classes work as expected under various conditions, which helps to catch errors early and facilitates easier maintenance and refactoring.

## How to:
Python comes with a built-in module for writing tests called `unittest`. This is how you can use it to test a simple function:

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "Should be 12")

if __name__ == '__main__':
    unittest.main()
```

When you run this test script, you should see output indicating that your tests passed (or failed).

For more modern and expressive tests, you can use a third-party library like `pytest`. First, you'll have to install it using pip:

```shell
pip install pytest
```

Then, you can write your tests in a simpler way without needing to subclass anything:

```python
# Save this in a file named test_with_pytest.py
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "Should be 12"
```

To run your tests with `pytest`, simply execute:

```shell
pytest test_with_pytest.py
```

You should see output from pytest showing your test results.
