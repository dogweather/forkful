---
title:                "Fish Shell: Znajdowanie długości ciągu znaków"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu potrzebujemy sprawdzić długość tekstu, czy to do celów wyświetlania, czy do manipulowania nim. W Fish Shell jest wiele sposobów na znalezienie długości ciągu tekstowego. W tym artykule dowiesz się, jak to zrobić.

## Jak to zrobić

Istnieją dwa sposoby na znalezienie długości ciągu tekstowego w Fish Shell. Pierwszym sposobem jest użycie wbudowanej funkcji `count`:

```
Fish Shell Count

fish
 
# 4
```

Drugi sposób polega na użyciu flagi `--count` w poleceniu `string`.

```
Fish Shell String Length

set string "fish"

string --count $string
 
# 4
```

Oba te sposoby zwracają liczbę znaków w podanym ciągu tekstowym.

## Deep Dive

Jeśli chcesz poznać więcej o znajdowaniu długości ciągu tekstowego w Fish Shell, warto wiedzieć, że można również użyć flagi `-r` w poleceniu `string` do odwrócenia podanego ciągu przed policzeniem długości. Ponadto, jeśli chcesz obliczyć długość wielu ciągów tekstowych na raz, możesz użyć polecenia `string split` w połączeniu z funkcją `count`.

Sprawdzanie długości ciągu tekstowego może również być przydatne w tworzeniu warunków w skryptach Fish Shell.

## Zobacz również

* Dokumentacja Fish Shell: https://fishshell.com/docs/current/
* Funkcja `count`: https://fishshell.com/docs/current/cmds/count.html
* Polecenie `string`: https://fishshell.com/docs/current/cmds/string.html
* Polecenie `string split`: https://fishshell.com/docs/current/cmds/string-split.html