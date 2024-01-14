---
title:                "Fish Shell: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Znajdowanie długości ciągu znaków jest ważnym aspektem programowania. Może to być przydatne do sprawdzania poprawności wprowadzanych danych, tworzenia pętli lub podczas manipulacji tekstem. W tym artykule opowiemy o tym, jak znaleźć długość ciągu znaków w języku Fish Shell.

## Jak to zrobić

Aby znaleźć długość ciągu znaków w języku Fish Shell, musimy skorzystać z wbudowanej funkcji `string length`, która zwraca liczbę znaków w podanym ciągu. Przykładowe użycie tej funkcji wygląda następująco:

```
Fish Shell> string length "Hello World"
11
```

Jak widać, funkcja zwróciła liczbę 11, co jest dokładną długością naszego ciągu znaków "Hello World". Możemy także wykorzystać zmienną reprezentującą ciąg znaków, a nie podawać go bezpośrednio w funkcji. Na przykład:

```
Fish Shell> set greeting "Cześć!"
Fish Shell> string length $greeting
6
```

W ten sposób możemy w łatwy sposób znaleźć długość dowolnego ciągu znaków w języku Fish Shell.

## Deep Dive

W języku Fish Shell możemy także wykorzystać efektywne i krótkie odwołania do elementów znajdujących się w naszym ciągu znaków. Na przykład, aby otrzymać ostatni znak w ciągu, możemy wykorzystać składnię `[-1]`, jak w przypadku poniższego przykładu:

```
Fish Shell> set word "Dzień dobry!"
Fish Shell> echo $word[-1]
!
```

Podobnie, aby otrzymać pierwszy znak, możemy skorzystać z `[1]`:

```
Fish Shell> echo $word[1]
D
```

W ten sposób możemy łatwo przeglądać i manipulować znakami w naszym ciągu znaków.

## Zobacz też
- [Dokumentacja języka Fish Shell](https://fishshell.com/docs/current/)
- [Przydatne komendy w języku Fish Shell](https://fishshell.com/docs/current/commands.html)