---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Czytanie argumentów z linii poleceń to metoda, dzięki której programy mogą przyjmować parametry bezpośrednio po uruchomieniu. Programiści robią to, aby rozbudować funkcjonalność i elastyczność swojego kodu, umożliwiając użytkownikowi dostosowanie działania programu.

## Jak to zrobić:
Pisać w Fish Shell jest wyjątkowo proste. Przykład czytania argumentów z linii poleceń prezentuje się następująco:

```fish
function powitanie
    echo Witaj, $argv[1]
end
```

A teraz uruchom powyższą funkcję:

```shell
$ powitanie Jan
Witaj, Jan
```

## Głębsze zanurzenie:
Czytanie argumentów z linii poleceń istnieje od początków informatyki, zapewniając elastyczność w interakcji z programami.

Alternatywą dla Fish jest Bash, lecz Fish oferuje bardziej nowoczesne i uproszczone podejście do składni.

W Fish możemy przechodzić przez wszystkie argumenty przy użyciu pętli:

```fish
function powitanie
    for imie in $argv
        echo Witaj, $imie
    end
end
```

Załóżmy, że mamy więcej niż jedną osobę:

```shell
$ powitanie Jan Anna Michał
Witaj, Jan
Witaj, Anna
Witaj, Michał
```

## Zobacz również:
Dodatkowe źródła, które mogą Ci pomóc:
1. Dokumentacja Fish Shell: https://fishshell.com/docs/current/
2. Stackoverflow - Fish: https://stackoverflow.com/questions/tagged/fish