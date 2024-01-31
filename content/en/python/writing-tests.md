---
title:                "Writing tests"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests means crafting code to check if other code works right. We do it to catch bugs, ensure reliability, and make updates less scary.

## How to:

Let's use Python's built-in `unittest` framework.

```Python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add_integers(self):
        self.assertEqual(add(1, 2), 3)

    def test_add_strings(self):
        self.assertEqual(add('abc', 'def'), 'abcdef')

if __name__ == '__main__':
    unittest.main()
```

Run it, you'll see something like:

```
..
----------------------------------------------------------------------
Ran 2 tests in 0.001s

OK
```

Two dots mean two tests passed. All's good.

## Deep Dive

Python testing started getting big with `unittest` (inspired by Java's JUnit). Now, there's `pytest` and `nose`, more modern tools with simpler syntax and better features. When writing tests, remember: isolate test cases, test edge cases, and mock external dependencies to focus on your code's logic, not the outside world.

## See Also

Dig deeper into testing with these:

- Python's `unittest` docs: https://docs.python.org/3/library/unittest.html
- `pytest` for a more modern approach: https://docs.pytest.org/en/latest/
- Mocking in tests with `unittest.mock`: https://docs.python.org/3/library/unittest.mock.html
