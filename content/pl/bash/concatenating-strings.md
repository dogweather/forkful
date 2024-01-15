---
title:                "Łączenie ciągów znaków"
html_title:           "Bash: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Konkatenacja to proces łączenia dwóch lub więcej ciągów znaków w jeden dłuższy ciąg. Jest to przydatna umiejętność w programowaniu, ponieważ pozwala na łatwe tworzenie wyświetlania komunikatów, generowanie plików lub budowanie bardziej złożonych programów.

## Jak to zrobić

Łączenie ciągów znaków w Bash jest łatwe i wymaga tylko użycia operatora "+" lub użycia komendy "echo". Przykłady:

```Bash
# Użycie "+" operatora
imie="John"
nazwisko="Smith"
pelne_imie=$imie+$nazwisko
echo "Witaj, $pelne_imie"

# Użycie komendy "echo"
echo "Dzisiejsza data to: " `date`
```

Wynik:

```
Witaj, John+Smith
Dzisiejsza data to: Fri Jan 15 20:20:20 CET 2021
```

## Deep Dive

Bash oferuje również możliwość konkatenacji ciągów znaków z wykorzystaniem funkcji "printf". Funkcja ta pozwala na bardziej zaawansowane formatowanie wyjścia, co może być przydatne przy generowaniu bardziej skomplikowanych ciągów znaków. Przykład:

```Bash
imie="John"
nazwisko="Smith"
printf "Witaj, %s %s" $imie $nazwisko
```

Wynik:

```
Witaj, John Smith
```

## Zobacz również

- ["Bash Concatenation Operator (+) and String Concatenation"] (https://www.oreilly.com/library/view/bash-cookbook/0596526784/ch01s15.html)
- ["Some Useful Tips and Tricks in Bash Programming"] (https://linuxacademy.com/blog/linux/some-useful-tips-and-tricks-in-bash-programming/)