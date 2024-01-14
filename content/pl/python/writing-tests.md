---
title:                "Python: Pisanie testów"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego?

Testowanie kodu jest jedną z najważniejszych praktyk w programowaniu. Pomaga ono w zapewnieniu poprawności i niezawodności naszego kodu oraz umożliwia łatwiejsze odnalezienie i naprawienie błędów. Pisząc testy, mamy również pewność, że nasze zmiany w kodzie nie wpływają negatywnie na działanie już istniejącego kodu. Dlatego warto poznać podstawy tworzenia testów w języku Python.

## Jak to zrobić?

Aby rozpocząć tworzenie testów w języku Python, potrzebujemy biblioteki o nazwie `unittest`. Poniżej przedstawiamy przykładowy kod, który testuje funkcję dodawania:

```Python
import unittest

def dodaj(a, b):
    return a + b

class TestyFunkcjiDodawania(unittest.TestCase):
    def test_poprawny_wynik(self):
        self.assertEqual(dodaj(2, 2), 4)

    def test_niepoprawny_wynik(self):
        self.assertNotEqual(dodaj(5, 5), 12)

if __name__ == '__main__':
    unittest.main()
```

Powyższy kod definiuje testową klasę `TestyFunkcjiDodawania`, która dziedziczy po klasie `unittest.TestCase`. Następnie definiujemy dwa testy - `test_poprawny_wynik`, który sprawdza, czy funkcja `dodaj` zwróciła poprawny wynik, oraz `test_niepoprawny_wynik`, w którym oczekujemy, że funkcja `dodaj` zwróci inny wynik niż oczekiwany. Na końcu wywołujemy funkcję `unittest.main()` w celu uruchomienia testów. 

Wynik wywołania powyższego kodu będzie wyglądał następująco:

```
..
----------------------------------------------------------------------
Ran 2 tests in 0.000s

OK
```

Kropki oznaczają, że wszystkie testy przeszły pomyślnie. Jeśli któryś z testów zawiedzie, zostanie wyświetlony błąd, który pomoże nam w odnalezieniu błędu w naszej funkcji.

## Głębszy zanurzenie

Tworzenie testów w języku Python jest bardzo elastyczne i pozwala na różnorodne podejścia. Możemy testować pojedyncze funkcje, klasy, a nawet całe aplikacje. Istnieje również wiele dodatkowych bibliotek, takich jak `pytest` czy `nose`, które oferują dodatkowe funkcjonalności i ułatwienia w pisaniu testów.

Jedną z najważniejszych koncepcji w testowaniu jest tzw. "jednostka testowa" - czyli najmniejsza możliwa część kodu, która może zostać przetestowana niezależnie. Dzięki temu, testy są bardziej przejrzyste i łatwiejsze w utrzymaniu.

Kolejną ważną częścią pisania testów jest pokrycie kodu - czyli odsetek kodu, który został przetestowany. W idealnym przypadku powinniśmy przetestować wszystkie części naszego kodu, jednak często jest to niemożliwe lub nieopłacalne. Dlatego ważne jest, aby wybierać najważniejsze funkcje i klasy do przetestowania.

## Zobacz też

- [Dokumentacja `unittest` w języku Python](https://docs.python.org/3/library/unittest.html)
- [Pytest - alternatywna biblioteka do testowania kodu w języku Python](https://docs.pytest.org/en/stable/)