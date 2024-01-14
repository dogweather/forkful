---
title:    "Gleam: Konwersja daty na ciąg znaków"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest niezbędnym elementem w wielu projektach programistycznych. Dzięki niej możemy wyświetlać daty w czytelny sposób, a także łatwiej porównywać i sortować dane. W tym wpisie dowiesz się, jak w prosty sposób wykonać konwersję datek w języku Gleam.

## Jak to zrobić

Aby dokonać konwersji daty na ciąg znaków w Gleam, musimy użyć wbudowanego modułu `Date` oraz funkcji `format`. Poniżej przedstawiamy przykładowy kod oraz jego wynik w formacie Markdown.

```Gleam
import Date

let date = Date.now()
let formatted_date = Date.format(date, "%d/%m/%Y")

assert formatted_date == "02/05/2021"
```

Jak widać powyżej, najpierw importujemy moduł `Date`, a następnie korzystając z funkcji `format`, przekazujemy do niej aktualną datę oraz format, w jakim chcemy ją wyświetlić. Pamiętaj, że format musi być zgodny z dokumentacją języka Gleam i mogą wystąpić różnice w stosunku do innych języków programowania.

## Deep Dive

Aby lepiej zrozumieć proces konwersji daty na ciąg znaków w języku Gleam, warto przejrzeć oficjalną dokumentację modułu `Date`. W nim znajdziesz pełną listę dostępnych formatów, a także dodatkowe funkcje, które mogą być przydatne w bardziej zaawansowanych przypadkach. Warto również pamiętać, że moduł ten jest ciągle rozwijany i możliwe, że w przyszłości pojawi się więcej opcji.

## Zobacz również

- Dokumentacja modułu `Date`: https://gleam.run/modules/date
- Przykładowe projekty w języku Gleam: https://github.com/search?q=language%3Agleam&type=Repositories