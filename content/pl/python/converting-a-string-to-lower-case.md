---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "Python: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja tekstu na małe litery jest powszechnym zadaniem w wielu projektach programistycznych. Często jest to wymagane w celu ułatwienia porównywania i przetwarzania danych. W tym artykule dowiesz się, jak można wykonać tę operację w języku Python.

## Jak to zrobić

Konwersja tekstu na małe litery w Pythonie jest bardzo prosta i może być wykonana na różne sposoby. Oto kilka przykładowych metod:

1. Metoda `lower()` - jest to metoda, która jest dostępna dla każdego obiektu typu `string` w Pythonie. Wywołanie tej metody na danym tekście spowoduje, że wszystkie jego litery zostaną zamienione na małe.

```Python
tekst = "HELLO WORLD"
print(tekst.lower())
```

Wynik: `hello world`

2. Metoda `casefold()` - jest to podobna metoda do `lower()`, która dodatkowo umożliwia obsługę specyficznych przypadków językowych, takich jak niemiecki znak "ß".

```Python
tekst = "STRAßE"
print(tekst.casefold())
```

Wynik: `strasse`

3. Wyrażenie `str.lower()` - jest to wygodna alternatywa dla metody `lower()`, ponieważ umożliwia bezpośrednie użycie jej na zmiennej typu `string`.

```Python
tekst = "WELCOME"
print(tekst.lower())
```

Wynik: `welcome`

## Deep Dive

Konwersja tekstu na małe litery może wydawać się prostym zadaniem, ale warto znać kilka dodatkowych informacji na temat tej operacji. Po pierwsze, warto zwrócić uwagę, że metoda `lower()` nie zmienia oryginalnej wartości tekstu, tylko zwraca nowy obiekt. W związku z tym, jeśli chcesz zachować zmiany w oryginalnym tekście, musisz przypisać wynik metody do nowej zmiennej.

Ponadto, sposób, w jaki metoda `lower()` zamienia litery, zależy od ustawień językowych Twojego systemu. Dla niektórych języków, takich jak turecki czy grecki, konwersja na małe litery może spowodować nieprawidłowe wyświetlanie znaków. W takich przypadkach, zaleca się użycie metody `casefold()` lub biblioteki `unicodedata`, która umożliwia bardziej precyzyjne kontrole nad konwersją.

## Zobacz także

- Oficjalna dokumentacja Pythona dotycząca konwersji tekstu na małe litery: https://docs.python.org/3/library/stdtypes.html#str.lower
- Wprowadzenie do wyrażeń regularnych w Pythonie: https://www.programiz.com/python-programming/regex