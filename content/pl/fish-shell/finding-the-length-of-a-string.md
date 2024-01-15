---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Fish Shell: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy zastanawiałeś się kiedykolwiek, jak możesz znaleźć długość ciągu znaków w Fish Shell? Może chcesz sprawdzić, czy wprowadzona przez użytkownika wartość nie jest za długa lub po prostu chcesz policzyć ilość znaków w swoim tekście. W tym artykule pokażę Ci, jak to zrobić w prosty sposób.

## Jak to zrobić

```Fish Shell
set string "Przykładowy ciąg znaków"
echo (string | wc -m)
```
Output:
`24`

Powyższy kod wykorzystuje wbudowane polecenie `wc`, które liczy ilość znaków w podanym ciągu lub pliku. Aby uzyskać długość ciągu, należy przekierować go do `wc` za pomocą operatora `|` i wykorzystać flagę `-m`, która oznacza wyświetlenie tylko liczby znaków. Następnie wykorzystujemy polecenie `echo` do wyświetlenia wyniku.

## Deep Dive

Jeśli chcesz dowiedzieć się więcej o tym temacie, warto poznać inne flagi dostępne w poleceniu `wc`. Flagi te umożliwiają np. liczenie słów (`-w`) lub linii (`-l`). Możesz również zapoznać się z dodatkowymi opcjami wyświetlania wyników lub różnymi sposobami przekierowania danych do `wc`. Nauka o tych możliwościach może pomóc Ci w lepszym wykorzystaniu tego polecenia w przyszłości.

## Zobacz także

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Przewodnik po podstawowych poleceniach w Fish Shell](https://devhints.io/fish)