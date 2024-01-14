---
title:    "Python: Pisanie testów"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Dlaczego pisanie testów jest ważne

Pisanie testów jest nieodłączną częścią procesu tworzenia oprogramowania. Jest to nie tylko dobry nawyk, ale również daje pewność, że napisany przez nas kod działa poprawnie i spełnia wymagane funkcjonalności. Testy dają nam również możliwość szybkiego wykrywania błędów i ułatwiają późniejszą refaktoryzację kodu.

## Jak pisać testy w Pythonie

Pisząc testy w Pythonie, możemy skorzystać z modułu `unittest`, który znajduje się w standardowej bibliotece języka. Poniżej przedstawimy przykładowy kod testu i jego wynik:

```Python
import unittest

def dodaj_liczby(a, b):
    return a + b

class TestSumy(unittest.TestCase):
    
    def test_dodawania_liczb_dodatnich(self):
        wynik = dodaj_liczby(5, 7)
        self.assertEqual(wynik, 12)
        
    def test_dodawania_liczb_ujemnych(self):
        wynik = dodaj_liczby(-10, 5)
        self.assertEqual(wynik, -5)
        
    def test_dodawania_liczb_zmiennoprzecinkowych(self):
        wynik = dodaj_liczby(3.5, 2.5)
        self.assertAlmostEqual(wynik, 6, delta=0.01)
```

Wynik działania testów:

```
----------------------------------------------------------------------
Ran 3 tests in 0.001s

OK
```

Powyższy kod zawiera trzy przykładowe testy dla funkcji `dodaj_liczby()`, sprawdzające jej działanie dla różnych typów argumentów. Używając `unittest` możemy stworzyć wiele takich testów dla każdej funkcji naszego programu, co znacznie zwiększa pewność i jakość naszego kodu.

## Głębsze spojrzenie w pisanie testów

Pisanie testów może być również postrzegane jako część konkretnego podejścia do programowania, jakim jest Test Driven Development (TDD). Polega ono na pisaniu testów przed napisaniem właściwej implementacji funkcjonalności, co pomaga w precyzyjnym określeniu oczekiwanej funkcjonalności oraz w uniknięciu niepotrzebnego kodu.

Warto również pamiętać, że testy powinny być niezależne od siebie oraz powtarzalne. Używanie w nich tych samych danych testowych oraz wykorzystywanie mocków mogą znacznie przyspieszyć proces pisania i wykonywania testów.

## Zobacz także

- [Dokumentacja modułu unittest w języku polskim](https://docs.python.org/pl/3/library/unittest.html)
- [Artykuł o Test Driven Development (TDD) w Pythonie](https://devstyle.pl/2015/01/25/test-driven-development-python/)
- [Blog o testowaniu w języku Python](https://pythontesting.net/)
- [Inne metody testowania w Pythonie](https://www.fullstackpython.com/testing.html)