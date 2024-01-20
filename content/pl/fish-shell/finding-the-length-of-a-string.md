---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Długość łańcucha to liczba znaków w danym łańcuchu tekstu. Programiści często muszą znać długość łańcucha, na przykład do walidacji danych wejściowych lub manipulacji tekstami.

## Jak to zrobić:

Aby znaleźć długość łańcucha w Fish Shell, możemy użyć wbudowanej funkcji `string length`. Oto prosty przykład:

```Fish Shell
string length -q "Twoj tekst"
```

Output:

```Fish Shell
11
```

## Głębsze Zanurzenie:

Historia funkcji `string length` w Fish nie jest skomplikowana, jest to przede wszystkim prosty moduł do manipulowania łańcuchami. Alternatywą są inne powłoki, takie jak Bash, które używają składni `${#zmienna}` do uzyskania długości łańcucha.

Jeśli chodzi o szczegóły implementacji, funkcja `string length` wykorzystuje standardowy algorytm do pomiaru długości łańcucha - iteruje przez każdy znak w łańcuchu, zliczając je aż do końca.

## Zobacz Również:

1. [Oficjalna dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
2. [Poradnik do manipulacji łańcuchami w Fish](https://github.com/jorgebucaran/fish-cookbook#readme)
3. [Bash vs Fish: porównanie powłoki](https://www.slant.co/versus/2443/2446/~bash_vs_fish)