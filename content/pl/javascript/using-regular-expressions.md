---
title:                "Używając wyrażeń regularnych"
html_title:           "Javascript: Używając wyrażeń regularnych"
simple_title:         "Używając wyrażeń regularnych"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Używanie wyrażeń regularnych jest niezwykle przydatne dla programistów, którzy chcą wykonywać operacje na łańcuchach znaków w sposób bardziej złożony i precyzyjny niż standardowe metody obsługi tekstu.

## Jak to zrobić

Użycie wyrażeń regularnych w języku Javascript jest bardzo proste. Wystarczy użyć obiektu `RegExp` oraz odpowiednich metod `test()` lub `exec()`. Przykładowy kod wyglądałby następująco:

```Javascript
let regex = new RegExp("kot", "i"); // tworzy nowy obiekt wyrażenia regularnego, przyjmując dwa argumenty - wzorzec oraz opcjonalne flagi (w tym przypadku "i" oznaczające ignorowanie wielkości liter)
let string = "Kot to najlepszy przyjaciel człowieka."; // przykładowy tekst, na którym będziemy operować
console.log(regex.test(string)); // sprawdzamy czy nasz wzorzec pasuje do tekstu (wynik: true)
console.log(regex.exec(string)); // wyświetlamy tablicę z informacjami o dopasowaniu (wynik: ["Kot", index: 0, input: "Kot to najlepszy przyjaciel człowieka.", groups: undefined])
```

Zauważ, że w naszym przykładzie użyliśmy flagi "i" aby zignorować wielkość liter, dzięki czemu wyrażenie regularne dopasowało zarówno "Kot" jak i "kot".

## Zagłębienie

Wyrażenia regularne posiadają wiele innych możliwości, takich jak wykorzystywanie wildcardów (znak `.`), alternatyw (znak `|`) czy kwantyfikatorów (np. `?` oznaczający opcjonalne dopasowanie). Warto przeszukać dostępne dokumentacje i przykłady, aby poznać pełny zakres funkcjonalności wyrażeń regularnych w języku Javascript.

## Zobacz także

- [Dokumentacja wyrażeń regularnych w języku Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Interaktywny kurs wyrażeń regularnych](https://regexone.com/)
- [Strona z poradnikami i przykładami wyrażeń regularnych](https://www.regular-expressions.info/)