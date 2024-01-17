---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Elm: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Co to jest i po co to?:

Znajdowanie długości napisu jest jednym z podstawowych zadań programistycznych. Polega ono na określeniu liczby znaków w ciągu tekstu. Jest to bardzo przydatne narzędzie w manipulowaniu i analizowaniu danych tekstowych. Szczególnie przydatne jest przy pisaniu aplikacji internetowych, gdzie często konieczne jest sprawdzenie, czy wprowadzony tekst spełnia wymagane przez nas kryteria.

Jak to zrobić:

W Elm istnieje prosty sposób na znalezienie długości napisu. Wystarczy użyć funkcji `String.length`, która przyjmuje ciąg tekstu jako argument i zwraca jego długość jako liczbę. Przykładowy kod może wyglądać tak:

```
Elm String.length "Hello World!" -- wynik: 12
Elm String.length "Lorem ipsum dolor sit amet" -- wynik: 26
```

Od razu widać, jak prosty i intuicyjny jest ten proces w Elm. Wynik zwracany przez funkcję jest dokładny i przewidywalny. W przeciwieństwie do innych języków programowania, nie ma konieczności definiowania dodatkowych zmiennych czy operacji, aby uzyskać długość napisu.

Głębokie nurkowanie:

Historia używania długości napisu sięga czasów maszyn liczących, gdzie była to jedna z podstawowych operacji. W dzisiejszych czasach znalezienie długości napisu jest jeszcze prostsze dzięki gotowym funkcjom w językach programowania. Jednym z popularnych sposobów na wykonanie tego zadania jest użycie pętli i inkrementacji licznika za każdym razem, gdy natrafimy na kolejny znak w ciągu. W Elm jednak możemy wykorzystać gotową funkcję, która znacznie skraca ten proces.

Alternatywnym sposobem na znalezienie długości napisu jest użycie metody `length` dostępnej w typie `String` w języku JavaScript. W Elm nie jest to jednak zalecane, ponieważ jest to funkcja skrywana przez kompilator i może powodować nieprzewidziane zachowania.

See also:

Jeśli chcesz dowiedzieć się więcej o funkcji `String.length` w Elm, możesz przejrzeć dokumentację języka pod tym linkiem: https://package.elm-lang.org/packages/elm/core/latest/String#length. Dodatkowo, jeśli interesują Cię inne operacje na tekstach w Elm, warto zapoznać się ze wspomnianym wcześniej typem `String`, który zawiera wiele przydatnych funkcji.