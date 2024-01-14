---
title:                "Fish Shell: Łączenie łańcuchów znaków"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Złączanie ciągów znaków jest powszechnym zadaniem w programowaniu. Pozwala na łączenie różnych elementów tekstu w jeden większy ciąg, co jest przydatne w wielu przypadkach. W tym artykule dowiesz się, jak to zrobić w środowisku Fish Shell.

## Jak

Do złączania ciągów używamy komendy `string join`. Poniżej znajdują się przykładowe kody oraz wyniki wykorzystania tej komendy w środowisku Fish Shell. 

```
Fish Shell - połączone ciągi znaków przy użyciu "string join"
```

```
string join ";" "Kot" "łapie" "myszy"   # Output: Kot;łapie;myszy
```

```
string join " " "Witaj" "na" "świecie"   # Output: Witaj na świecie
```

Możesz również używać zmiennej do złączania ciągów. Przykładowo:

```
set kolor "niebieski"
set predmiot "pióro"
string join " " $kolor $predmiot   # Output: niebieski pióro
```

## Deep Dive

Funkcja `string join` pozwala na łączenie ciągów znaków przy użyciu separatora. Można to zrobić poprzez podanie separatora jako pierwszego argumentu w komendzie. Jeśli nie zostanie on podany, domyślnym separatorem jest spacja.

Innym sposobem złączania ciągów jest użycie pętli `for`. Przykładowo, jeśli mamy listę przedmiotów, możemy połączyć je w jeden dłuższy ciąg używając pętli oraz funkcji `string join`. Poniżej znajduje się kod oraz wynik wykorzystania tego sposobu:

```
set przedmioty "długopis" "zeszyt" "linijka"
for przedmiot in $przedmioty
    string join ", " $przedmiot  # Output: długopis, zeszyt, linijka
end
```

## Zobacz również

- Dokumentacja Fish Shell: https://fishshell.com/docs/current/index.html
- Przykłady użycia funkcji `string join`: https://fishshell.com/docs/current/cmds/string.join.html
- Inne przydatne komendy w środowisku Fish Shell: https://fishshell.com/docs/current/cmds/