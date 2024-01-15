---
title:                "Szukanie długości ciągu znaków"
html_title:           "Elm: Szukanie długości ciągu znaków"
simple_title:         "Szukanie długości ciągu znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak obliczyć długość tekstu w programowaniu w Elm? Niezależnie od tego, czy tworzysz stronę internetową, aplikację mobilną czy grę, znajomość długości tekstu jest niezbędna w wielu zastosowaniach. W tym artykule dowiesz się, jak można łatwo i szybko znaleźć długość tekstu w programowaniu w Elm.

## Jak to zrobić

```Elm
stringLength : String -> Int
stringLength text =
  String.length text
```

Kod powyżej pokazuje prostą funkcję, która przyjmuje wartość tekstową i zwraca jej długość jako wartość całkowitą. Jest to możliwe dzięki wbudowanej funkcji "String.length", która zwraca długość tekstu przekazanego do niej jako argument. Dzięki temu możesz łatwo obliczyć długość tekstu w swoich projektach w Elm.

Przykład wykorzystania tej funkcji:

```Elm
text = "Cześć, jak się masz?"
długośćTekstu = stringLength text
```

W powyższym przykładzie, wynikiem funkcji "stringLength" będzie liczba 18, ponieważ tyle znaków zawiera tekst "Cześć, jak się masz?". Teraz już wiesz, jak wykorzystać wbudowaną funkcję "String.length" do obliczania długości tekstu w programowaniu w Elm.

## Deep Dive

Funkcja "String.length" jest jedną z wielu funkcji dostępnych w Elm do manipulacji tekstem. Poza "String.length" istnieją również inne funkcje, takie jak "String.isEmpty" do sprawdzania czy tekst jest pusty, "String.concat" do łączenia kilku tekstów w jeden oraz "String.contains" do sprawdzania, czy dany tekst zawiera inny tekst. Więcej informacji na temat tych i innych funkcji możesz znaleźć w dokumentacji języka Elm.

Więcej informacji na temat operacji na tekście w Elm można również znaleźć na różnych stronach internetowych, takich jak:

- Oficjalna dokumentacja Elm: https://elm-lang.org/docs
- Elm School: https://courses.knowthen.com/courses/elm-fundamentals
- Elm by Example: http://elmbyexample.com/

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o programowaniu w Elm, polecamy odwiedzić poniższe strony:

- Official Elm Guide: https://guide.elm-lang.org/
- Learn Elm in Y minutes: https://learnxinyminutes.com/docs/elm/
- Elm Community: https://elm-community.org/