---
title:                "Python: Wydrukowanie wyników debugowania"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

W trakcie programowania, debugowanie jest jednym z najważniejszych procesów mających na celu poprawienie błędów w kodzie. Jedną z najprostszych i najczęściej stosowanych metod debugowania jest wyświetlanie informacji zwrotnych (debug output) na przestrzeni działania programu. W tym artykule dowiesz się, dlaczego warto korzystać z tej metody oraz jak to zrobić.

## Jak to zrobić

Wyświetlanie debug outputu w Pythonie jest bardzo proste. Wystarczy użyć funkcji `print()` i podać w niej odpowiednie wartości lub zmienne. Przykładowy kod wyglądałby następująco:

```Python
x = 5
y = 10
z = x + y

print("Wartość zmiennej z wynosi:", z)
```

Powyższy kod wyświetli nam na konsoli napis "Wartość zmiennej z wynosi: 15". Jest to bardzo przydatne podczas debugowania, ponieważ możemy śledzić wartości zmiennych i sprawdzać, czy nasz kod działa zgodnie z oczekiwaniami.

Możemy również wyświetlić więcej informacji za pomocą formatowania napisów. W tym celu używamy `f-strings` oraz wyrażenia `{}` wewnątrz napisu, do którego chcemy dodać nasze zmienne. Przykładowo:

```Python
imie = "Jan"
nazwisko = "Kowalski"

print(f"Moje imie to {imie} a nazwisko {nazwisko}")
```

Wynikiem będzie napis "Moje imię to Jan, a nazwisko Kowalski".

## Deep Dive

Istnieje wiele innych sposobów wyświetlania debug outputu w Pythonie, takich jak moduł `logging` czy function `repr()`. Jednakże, najważniejsze jest to, aby wybrać metodę, która najlepiej pasuje do naszych potrzeb i jest łatwa do zrozumienia dla nas oraz dla innych osób, które będą później czytać nasz kod.

Niektóre z zalet wyświetlania debug outputu to:

- Możliwość śledzenia wartości zmiennych i wykrywania błędów w kodzie
- Prostota implementacji i korzystania z tej metody
- Ułatwienie pracy w zespole i dzielenia się kodem z innymi osobami

## Zobacz także

- [Śledzenie błędów w Pythonie przy użyciu debuggera](https://realpython.com/python-debugging-pdb/)
- [Tutoriale Pythona o debugowaniu i wyświetlaniu informacji zwrotnych](https://realpython.com/python-debugging-pdb/)
- [Dokumentacja Pythona o module logging](https://docs.python.org/3/library/logging.html)