---
date: 2024-02-03 19:03:27.970246-07:00
description: 'How to: Python comes with a built-in module for writing tests called
  `unittest`. This is how you can use it to test a simple function.'
lastmod: '2024-03-13T22:44:59.711376-06:00'
model: gpt-4-0125-preview
summary: Python comes with a built-in module for writing tests called `unittest`.
title: Writing tests
weight: 36
---

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
