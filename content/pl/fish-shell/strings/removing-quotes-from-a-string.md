---
date: 2024-01-26 03:39:16.776497-07:00
description: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w polega na pozbyciu\
  \ si\u0119 tych irytuj\u0105cych pojedynczych (' ') lub podw\xF3jnych (\" \") znak\xF3\
  w cudzys\u0142owu z danych tekstowych.\u2026"
lastmod: '2024-02-25T18:49:34.200178-07:00'
model: gpt-4-0125-preview
summary: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w polega na pozbyciu si\u0119\
  \ tych irytuj\u0105cych pojedynczych (' ') lub podw\xF3jnych (\" \") znak\xF3w cudzys\u0142\
  owu z danych tekstowych.\u2026"
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?

Usuwanie cudzysłowów z ciągu znaków polega na pozbyciu się tych irytujących pojedynczych (' ') lub podwójnych (" ") znaków cudzysłowu z danych tekstowych. Programiści często robią to, aby oczyścić dane wejściowe lub przygotować dane do dalszej obróbki bez bałaganu związanego z cudzysłowami.

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
