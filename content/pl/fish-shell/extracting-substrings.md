---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Czym i Dlaczego?
Ekstrakcja podciągów polega na wydostawaniu konkretnych fragmentów tekstu z większego ciągu. Programiści robią to, aby przetworzyć jedynie potrzebne im dane, ignorując nieistotne informacje.

## Jak to zrobić:
Łatwo jest ekstrahować podciągi w Fish Shell. Spójrz na poniższy przykład:
```Fish Shell
set napis "Cześć, jestem Fish Shell!"
echo $napis[1 6]
```
Wynik: 
```
Cześć, jestem
```
Tutaj polecenie `echo $napis[1 6]` wyszukuje i wyświetla pierwsze 6 znaków ciągu.

## Głębsze Zanurzenie:
Ekstrakcja podciągów jest tak stara jak samo programowanie. Inne języki, jak Python lub JavaScript, mają także funkcje do manipulowania ciągami. Najważniejsze, że w Fish Shell, podobnie jak w wielu nowoczesnych interfejsach powłoki, indeksowanie zaczyna się od 1, a nie od 0. 

W pewnych sytuacjach możesz chcieć użyć alternatywnych metod ekstrakcji podciągów, np. gdy przetwarzasz dane binarne lub skomplikowane wyrażenia regularne. Pamiętaj jednak, że wbudowane funkcje ekstrakcji podciągów są zwykle szybkie i wydajne.

## Zobacz też:
Chcesz się dowiedzieć więcej? Sprawdź poniższe linki:

- Podstawy stringów w Fish Shell: [Fish shell string fundamentals](https://fishshell.com/docs/current/cmds/string.html)