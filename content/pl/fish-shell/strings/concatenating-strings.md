---
title:                "Łączenie łańcuchów znaków"
aliases:
- /pl/fish-shell/concatenating-strings/
date:                  2024-01-20T17:34:35.722202-07:00
model:                 gpt-4-1106-preview
simple_title:         "Łączenie łańcuchów znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konkatenacja to fancy termin na łączenie stringów, tworzenie z nich dłuższego ciągu znaków. Programiści to robią, gdy potrzebują zbudować wartości, takie jak unikalne wiadomości, adresy URL, czy też po prostu wyświetlić coś ładnie dla użytkownika.

## Jak to zrobić:
Łączenie stringów w Fish można zrobić na kilka sposobów, oto najprostszy:

```Fish Shell
set string1 "Cześć, "
set string2 "jak się masz?"
set concatenated $string1$string2
echo $concatenated
```

Output:
```
Cześć, jak się masz?
```

Możesz też użyć polecenia `string`:

```Fish Shell
echo (string join '' $string1 $string2)
```

## Głębsze zanurzenie
Konkatenacja stringów to coś, co robiły języki programowania jeszcze przed epoką internetu. W Fish, odbywa się to bez większej ceremonii - przylep jeden string do drugiego i voilà! Alternatywnie, można użyć wbudowanej funkcji `string`, która oferuje dużo drobniejsze możliwości manipulacji stringami, w tym konkatenację.

Fish nie wymaga specjalnych operatorów do łączenia stringów, ale niektóre języki jak Python używają `+`, a PHP `.`. Taka różnorodność wynika z historii i filozofii projektowej poszczególnych języków.

## Zobacz również:
- Dokumentacja Fish `string`: https://fishshell.com/docs/current/cmds/string.html
- Przewodnik Fish dla początkujących: https://fishshell.com/docs/current/tutorial.html
- Wprowadzenie do Fish Shell: https://gist.github.com/krzysztofzuraw/6098499
