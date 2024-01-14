---
title:    "Python: Pisanie testów"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego warto pisać testy w Pythonie?

Pisanie testów to kluczowa część procesu programowania w Pythonie. Testy pozwalają nam na sprawdzenie poprawności działania naszego kodu oraz zapewniają większą pewność, że nasz program będzie działał zgodnie z oczekiwaniami. Jest to nie tylko wygodna metoda, ale również pozwala zaoszczędzić czas i uniknąć potencjalnych błędów w przyszłości.

## Jak pisać testy w Pythonie?

Aby rozpocząć pisanie testów w Pythonie, musimy zaimportować moduł `unittest` w naszym kodzie. Ten moduł zawiera narzędzia, które pozwolą nam na definiowanie testów oraz porównywanie oczekiwanych wyników z rzeczywistymi. Poniżej znajduje się przykładowy kod, który demonstruje jak używać modułu `unittest` do testowania funkcji dodawania:

```python
import unittest

# Funkcja dodająca dwie liczby
def dodaj(a, b):
    return a + b

# Klasa testowa
class TestDodawania(unittest.TestCase):

    # Metoda testująca poprawność wyniku dodawania
    def test_dodawania(self):
        wynik = dodaj(2, 3)
        self.assertEqual(wynik, 5) # Porównanie oczekiwanego wyniku z rzeczywistym

if __name__ == '__main__':
    unittest.main()
```

Po uruchomieniu powyższego kodu, otrzymamy informację, że nasz test został przeprowadzony pomyślnie. Jednak, jeśli zmienimy wynik funkcji `dodaj` na 6, otrzymamy informację o błędzie, co pokazuje nam, że nasz test zadziałał poprawnie i wykrył zmianę w kodzie.

## Głębszy zanurzenie w temat pisania testów

W Pythonie mamy dostępnych wiele rodzajów testów, takich jak testy jednostkowe, testy integracyjne czy testy wydajnościowe. Warto zapoznać się z nimi i wybrać te, które będą najbardziej przydatne w naszym projekcie. Ponadto, ważne jest aby pisać testy już na etapie tworzenia kodu, a nie dopiero po jego zakończeniu. Dzięki temu unikniemy konieczności przepracowywania naszego kodu w przypadku błędów, co zaoszczędzi nam czas i stres.

## Zobacz także

- [Dokumentacja modułu unittest](https://docs.python.org/3/library/unittest.html)
- [Poradnik dla początkujących w pisaniu testów w Pythonie](https://realpython.com/python-testing/)
- [Pytest - inny popularny moduł do testowania w Pythonie](https://docs.pytest.org/en/latest/)