---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Najprościej mówiąc, wyszukiwanie i zastępowanie to techniki na modyfikację tekstu przez znalezienie konkretnych fraz i zmianę ich na inne. Programiści to robią, by edytować kod, poprawiać błędy i przyspieszać pracę.

## Jak to zrobić:

Zastosujmy to w praktyce w Fish Shell:
```fish
set -l old_text 'stary tekst'
set -l new_text 'nowy tekst'
set -l target 'To jest mój stary tekst.'

echo $target | string replace -r $old_text $new_text
```
Gdy uruchomisz powyższy skrypt, wydrukuje on: 'To jest mój nowy tekst.'

## Głębsze spojrzenie

Narzędzie do wyszukiwania i zastępowania tekstu ma długą historię, zaczynającą się od starożytnych edytorów tekstowych, takich jak ed. W Fish Shell, wykorzystaliśmy wbudowaną funkcję `string replace`, która jest wynikiem rozwoju tych historycznych narzędzi.

Inne powszechne metody to używanie `sed` albo `awk`, które są bardziej złożone, lecz także bardziej elastyczne. Zakres ich możliwości daleko wykracza poza to, co prezentowane jest w tym artykule.

Ciekawe jest to, że `string replace` w Fish operuje na całych linijkach, a nie na strumieniach znaków. To oznacza, że nie możesz go użyć do modyfikacji tekstu przekraczającego granice linii - wtedy musisz skorzystać z innych narzędzi.

## Zobacz również:

- [Dokumentacja Fish Shell 'string replace'](https://fishshell.com/docs/3.0/cmds/string-replace.html)
- [Przewodnik po używaniu 'awk' w UNIX](https://www.grymoire.com/Unix/Awk.html)
- [Tutoriale 'sed' - Używanie 'sed' do wyszukiwania i zastępowania tekstu](https://how-to.fandom.com/wiki/How_to_use_sed_to_find_and_replace_text_in_files)