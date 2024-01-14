---
title:                "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu potrzebujemy zmienić stringi na małe litery, niezależnie od tego jak są wprowadzone. W tym wpisie dowiesz się, jak łatwo wykonać tę operację w języku Fish Shell.

## Jak to zrobić

Aby przekonwertować string na małe litery w Fish Shell, możemy skorzystać z funkcji `string tolower`, która zwraca przekonwertowany tekst. Oto przykładowy kod:

```Fish Shell
string tolower "JESTEM PRZESTRZEGANIEGŁUPICH zasad"
```

Wynik wyświetli się w postaci:

```Fish Shell
jestem przestrzeganiem głupich zasad
```

Jeśli chcemy zmienić podaną wartość bezpośrednio, możemy skorzystać z wyrażenia regularnego `string match`, które będzie przypisywać zmienną z małymi literami, jak w poniższym przykładzie:

```Fish Shell
string value "JAk nAkAżCiwy ZawARTość"
echo "$value" | string match -r ".*" --write-to value
echo "$value"
```

Powyższy kod wyświetli nam:

```Fish Shell
jak nakażciwy zawartość
```

## Deep Dive

Podczas wykorzystywania funkcji `string tolower` musimy być świadomi, że nie tylko zmienia się wielkość liter, ale również zmienia się struktura tekstu, a dokładniej modyfikuje się znaki niestandardowe i znaki diakrytyczne. Na przykład, litera `Ś` zostanie przekonwertowana na `ś`, a `ó` na `o`.

Jeśli chcemy uniknąć tych zmian, możemy skorzystać z funkcji `string lowercase`, która tylko zmieni wielkość liter, ale zostawi resztę tekstu bez zmian. Przykład użycia tej metody może wyglądać następująco:

```Fish Shell
string value "JAk nAkAżCiwy ZawARTość"
echo "$value" | string lowercase
```

Wynik będzie wyglądał identycznie jak w przykładzie dla funkcji `string match`.

## Zobacz też

- Dokumentacja Fish Shell na temat funkcji `string tolower`: https://fishshell.com/docs/current/cmds/tolower.html
- Źródło wyrażeń regularnych: https://www.regular-expressions.info/