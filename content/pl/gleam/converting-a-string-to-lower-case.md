---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?
Zamiana tekstu na małe litery to operacja polegająca na przekształcaniu wszystkich liter każdego wyrazu ze zbioru znaków na ich małe odpowiedniki. Programiści przeprowadzają tę operację, aby ułatwić porównywanie i sortowanie napisów, jak również usunąć możliwość pomyłki związanej z wielkością liter.

## Jak to zrobić:
Gleam, język programowania statycznego, umożliwia łatwe wywołanie tej funkcji. Poniżej znajduje się przykład przekształcania napisu na małe litery:

```gleam
import gleam/string

fn main() {
  let text = "Dzień Dobry, Świecie!"
  let lower_text = string.lower(text)
  println(lower_text)
}
```

Po wywołaniu programu otrzymasz:

```gleam
"dzień dobry, świece!"
```
## Pogłębienie
Zasada działania tej funkcji jest bardzo prosta. Gdy wprowadzamy tekst, program przechodzi przez każdą literę i zamienia jej ASCII lub Unicode na odpowiednik z małą literą. Początkowo funkcja ta powstała w C i została przeniesiona do większości języków programowania.

W niektórych językach, takich jak JavaScript, mamy alternatywne metody, takie jak `toLocaleLowerCase()`, które dodatkowo uwzględniają lokalizację.

Nie jest jednak zawsze konieczne przechodzenie przez całą procedurę konwersji, jeżeli wiemy, że nasz tekst jest już znormalizowany. W Gleam, funkcja string.lower jest zrealizowana z użyciem funkcji 'str.toLocaleLowerCase()' z JavaScript.

## Zobacz też:
1. [Dokumentacje Gleam (ang. Gleam Documentation)](https://gleam.run/docs/)
2. [Więcej o funkcji str.toLocaleLowerCase (ang. str.toLocaleLowerCase function on MDN)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)