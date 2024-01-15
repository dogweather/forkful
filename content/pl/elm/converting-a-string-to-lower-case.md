---
title:                "Zamiana ciągu znaków na małe litery"
html_title:           "Elm: Zamiana ciągu znaków na małe litery"
simple_title:         "Zamiana ciągu znaków na małe litery"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Cześć programiści! W dzisiejszym artykule rozwiniemy nasze umiejętności w Elm poprzez naukę przekształcania ciągu znaków w małe litery. To ważne narzędzie w tworzeniu aplikacji, ponieważ umożliwia sprawdzanie równoważności liter, co jest szczególnie przydatne przy weryfikacji haseł czy nazw użytkowników.

Przed przystąpieniem do kodowania, warto zaznaczyć, że w Elm nie można modyfikować istniejącego ciągu znaków, ponieważ jest on niezmienny (immutable). Zamiast tego, musimy przekształcić oryginalny ciąg znaków w nowy, zawierający odpowiednie litery.

## Jak to zrobić

W celu przekształcenia ciągu znaków w małe litery, musimy użyć funkcji `String.toLower`. Przyjrzyjmy się przykładom:

```Elm
import String

String.toLower "ELM" -- output: "elm"
String.toLower "Hello World!" -- output: "hello world!"
```

W pierwszym przykładzie przekształcamy ciąg "ELM" w "elm". W drugim natomiast, zmieniamy "Hello World!" na "hello world!".

Ważne jest również zwrócenie uwagi na znaki nietypowe, takie jak litery ze znakami diakrytycznymi czy spacje, które również są przekształcane na odpowiednie małe litery.

## Głębsze zagadnienia

W Elm, funkcja `String.toLower` wykorzystuje standard Unicode do transformacji liter. Oznacza to, że nie tylko litery z alfabetu łacińskiego są przekształcane, ale również znaki z innych języków.

Jedną z zalet używania funkcji `String.toLower` jest fakt, że ta sama funkcja może być stosowana zarówno do pojedynczego ciągu znaków, jak i listy ciągów. Dzieje się tak dzięki tzw. "funkcyjnemu programowaniu", które jest bardzo popularne w Elm.

## Zobacz również

Jeśli chcesz poznać więcej funkcji manipulujących ciągami znaków w Elm, polecam przeczytać ten artykuł: [Manipulowanie ciągami znaków w Elm](https://medium.com/@kanonzm/functional-programming-with-elm-string-manipulation-6f9fe0b350ba).

A jeśli jesteś ciekawy, jak działa funkcjonalne programowanie w Elm, zapraszam do zapoznania się z tym kursem: [Wprowadzenie do funkcyjnego programowania w Elm](https://egghead.io/browse/frameworks/elm).