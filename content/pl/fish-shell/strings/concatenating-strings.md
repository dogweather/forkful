---
date: 2024-01-20 17:34:35.722202-07:00
description: "Konkatenacja to fancy termin na \u0142\u0105czenie string\xF3w, tworzenie\
  \ z nich d\u0142u\u017Cszego ci\u0105gu znak\xF3w. Programi\u015Bci to robi\u0105\
  , gdy potrzebuj\u0105 zbudowa\u0107 warto\u015Bci, takie\u2026"
lastmod: '2024-03-11T00:14:09.043411-06:00'
model: gpt-4-1106-preview
summary: "Konkatenacja to fancy termin na \u0142\u0105czenie string\xF3w, tworzenie\
  \ z nich d\u0142u\u017Cszego ci\u0105gu znak\xF3w. Programi\u015Bci to robi\u0105\
  , gdy potrzebuj\u0105 zbudowa\u0107 warto\u015Bci, takie\u2026"
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
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
