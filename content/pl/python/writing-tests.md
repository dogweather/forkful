---
title:                "Pisanie testów"
html_title:           "Python: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-tests.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Pisanie testów jest nieodłączną częścią procesu programowania. Jest to proces weryfikacji czy kod programu działa poprawnie oraz spełnia oczekiwane wymagania. Programiści piszą testy, aby uniknąć błędów i zapewnić niezawodność swoich programów.

## Jak to zrobić:

```Python
# Importujemy moduł unittest, który pomaga nam w pisanie testów.
import unittest

# Tworzymy klasę Test, zawierającą funkcje testowe.
class Test(unittest.TestCase):
    # Funkcja sprawdzająca czy dwa wyrażenia są równe.
    def test_equal(self):
        self.assertEqual(1+2, 3)
        
    # Funkcja sprawdzająca czy wyrażenie jest prawdziwe.
    def test_true(self):
        self.assertTrue(5 > 3)
        
    # Funkcja sprawdzająca czy lista zawiera określony element.
    def test_in(self):
        my_list = [1, 2, 3]
        self.assertIn(2, my_list)
        
# Uruchamiamy testy.
if __name__ == '__main__':
    unittest.main()
```

**Wynik wykonania testów:**
```
..
----------------------------------------------------------------------
Ran 2 tests in 0.000s

OK
```

## Deep Dive:

Pierwsze testy pojawiły się w latach 50. XX wieku. Nazywali się "programistycznymi testami jednostkowymi" i były używane w programowaniu w języku Fortran. Obecnie, istnieje wiele różnych bibliotek, narzędzi i frameworków do pisania testów w języku Python, takich jak unittest, pytest czy doctest.

Alternatywnym podejściem do pisania testów jest "tdd" (test-driven development). Polega on na tym, aby najpierw napisać testy dla kodu, a dopiero potem napisać kod, który je przejdzie. Jest to popularna metoda wśród programistów zwłaszcza w metodyce Agile.

Podczas pisania testów ważne jest, aby tworzyć testy możliwie najbardziej dokładne i obejmujące wiele różnych przypadków, aby zapewnić kompleksowe testowanie kodu.

## Zobacz też:

- Dokumentacja unittest: https://docs.python.org/3/library/unittest.html
- Porównanie unittest i pytest: https://realpython.com/python-testing/
- Tutorial tdd w Pythonie: https://www.obeythetestinggoat.com/pages/book.html#toc