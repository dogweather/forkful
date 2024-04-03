---
date: 2024-01-26 03:39:16.776497-07:00
description: "Jak to zrobi\u0107: Fish ma wbudowane czary do tego rodzaju zada\u0144\
  . U\u017Cyj funkcji `string` bez wi\u0119kszego wysi\u0142ku. Sprawd\u017A te zakl\u0119\
  cia."
lastmod: '2024-03-13T22:44:35.826539-06:00'
model: gpt-4-0125-preview
summary: "Fish ma wbudowane czary do tego rodzaju zada\u0144."
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
weight: 9
---

## Jak to zrobić:
Fish ma wbudowane czary do tego rodzaju zadań. Użyj funkcji `string` bez większego wysiłku. Sprawdź te zaklęcia:

```fish
# Przykład z pojedynczymi cudzysłowami
set quoted "'Hello, World!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # Wyjście: Hello, World!

# To samo dotyczy podwójnych cudzysłowów
set double_quoted "\"Hello, Universe!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # Wyjście: Hello, Universe!
```

## Dogłębna eksploracja
W kamiennej erze linii poleceń musiałbyś zmagać się z `sed` lub `awk`, aby usunąć cudzysłowy; prawdziwy gąszcz ukośników wstecznych i tajemniczych flag. Funkcja `string` w Fish pochodzi z nowszej ery, sprawiając, że kod jest czyściejszy i bardziej intuicyjny.

Alternatywy w innych powłokach mogą nadal polegać na tych starych narzędziach lub mogą używać własnych wbudowanych metod, takich jak ekspansja parametrów w bashu czy modyfikatory w zsh.

Funkcja `string` wykracza poza same przycinanie cudzysłowów. To scyzoryk Szwajcarski do operacji na ciągach znaków w Fish. Dzięki `string`, możesz ciąć, dzielić, łączyć czy nawet dopasowywać wyrażenia regularne bezpośrednio w terminalu.

## Zobacz także
Pogłęb swoją wiedzę o `string` z pomocą oficjalnej dokumentacji:
- [Dokumentacja Fish Shell String](https://fishshell.com/docs/current/commands.html#string)

Do nostalgii lub kiedy piszesz skrypty w bardziej tradycyjnych powłokach, sprawdź:
- [Przewodnik po Sed & Awk](https://www.grymoire.com/Unix/Sed.html)
- [Ekspansja Parametrów w Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
