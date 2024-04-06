---
date: 2024-01-20 17:45:39.277809-07:00
description: "Jak to zrobi\u0107: Wyci\u0105ganie podci\u0105g\xF3w w Fish dzieje\
  \ si\u0119 za pomoc\u0105 wbudowanego polecenia `string`. Fish Shell, stworzony\
  \ w 2005 roku, postawi\u0142 na prostsz\u0105\u2026"
lastmod: '2024-04-05T21:53:37.255037-06:00'
model: gpt-4-1106-preview
summary: "Wyci\u0105ganie podci\u0105g\xF3w w Fish dzieje si\u0119 za pomoc\u0105\
  \ wbudowanego polecenia `string`."
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
weight: 6
---

## Jak to zrobić:
```Fish Shell
# Przykładowy ciąg znaków
set ciag "Fish Shell jest super!"

# Wyciągnięcie 'Shell' z ciągu znaków, od pozycji 6 do 10
echo $ciag | string sub -s 6 -l 5
# Wyjście: Shell

# Wyciągnięcie ostatnich 6 znaków
echo $ciag | string sub -s -6
# Wyjście: super!

# Wyekstrahowanie 'jest' używając wyrażeń regularnych
echo $ciag | string match -r -o 'jest'
# Wyjście: jest
```

## Deep Dive
Wyciąganie podciągów w Fish dzieje się za pomocą wbudowanego polecenia `string`. Fish Shell, stworzony w 2005 roku, postawił na prostszą składnię w porównaniu do tradycyjnych bash czy zsh. Alternatywą jest użycie `sed` lub `awk`, ale `string` jest bardziej intuicyjne. W implementacji Fish, polecenie `string` zaprojektowano tak, aby unikać niepotrzebnej złożoności, oferując czytelność i wygodę.

## Zobacz też:
- Dokumentacja Fish Shell `string`: https://fishshell.com/docs/current/cmds/string.html
- Tutaj o wyrażeniach regularnych w Fish: https://fishshell.com/docs/current/index.html#regular-expressions
- Porównanie Fish Shell do innych powłok: https://github.com/fish-shell/fish-shell/wiki/FAQ
