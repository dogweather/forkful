---
title:                "Refaktoryzacja"
date:                  2024-01-26T01:16:40.593487-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktoryzacja"
programming_language: "Bash"
category:             "Bash"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/refactoring.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Refaktoryzacja to proces restrukturyzacji istniejącego kodu komputerowego bez zmiany jego zewnętrznego zachowania. Jest to kluczowa praktyka, aby zmniejszyć złożoność, poprawić możliwość utrzymania kodu i zachować zdrową oraz łatwiejszą do zrozumienia bazę kodów dla obecnych i przyszłych programistów.

## Jak to zrobić:
Rozważmy prosty skrypt Bash, który wymaga refaktoryzacji. Jest nieporęczny, z powtarzającym się kodem i trudny do śledzenia:

```Bash
#!/bin/bash
echo "Wprowadź nazwę pliku:"
read filename
if [ -f "$filename" ]; then
    echo "Plik istnieje."
    count=$(grep -c "foo" "$filename")
    echo "Słowo foo pojawia się $count razy."
else
    echo "Plik nie istnieje."
fi
```

Refaktoryzacja pod kątem jasności i możliwości ponownego użycia może polegać na wprowadzeniu funkcji oraz bardziej eleganckim obsługiwaniu błędów:

```Bash
#!/bin/bash

function file_exists() {
    [[ -f "$1" ]]
}

function count_occurrences() {
    grep -c "$1" "$2"
}

function main() {
    local filename word count
    echo "Wprowadź nazwę pliku:"
    read -r filename
    echo "Wprowadź szukane słowo:"
    read -r word

    if file_exists "$filename"; then
        count=$(count_occurrences "$word" "$filename")
        echo "Słowo $word pojawia się $count razy."
    else
        echo "Plik nie istnieje." >&2
        exit 1
    fi
}

main "$@"
```

Wersja po refaktoryzacji wykorzystuje funkcje do poprawy czytelności i umożliwia potencjalne ponowne użytkowanie.

## Dogłębne zgłębienie:
Refaktoryzacja to nie pojęcie, które powstało wraz z Bashem czy nawet językami programowania wysokiego poziomu; jest tak stare jak programowanie samo w sobie. Termin został sformalizowany w książce "Refaktoryzacja: Ulepszanie projektu istniejącego kodu" autorstwa Martina Fowlera w 1999 roku, skupiając się głównie na językach zorientowanych obiektowo.

W kontekście skryptów Bash, refaktoryzacja często oznacza rozbijanie długich skryptów na funkcje, zmniejszanie powtórzeń za pomocą pętli lub instrukcji warunkowych oraz unikanie typowych błędów jak nieobsługiwanie białych znaków w nazwach plików. Alternatywami dla Bash, gdy skrypty stają się zbyt skomplikowane, są Python czy Perl, które oferują lepsze struktury danych i obsługę błędów do złożonych zadań.

Refaktoryzacja specyficzna dla Bash polega bardziej na przestrzeganiu najlepszych praktyk, takich jak używanie cudzysłowów wokół zmiennych, stosowanie `[[ ]]` do testów zamiast `[ ]`, oraz preferowanie `printf` nad `echo` dla bardziej solidnego wyjścia. Szczegóły implementacji często obracają się wokół przestrzegania poradników stylu i korzystania z narzędzi takich jak `shellcheck` do statycznej analizy w celu wykrywania typowych błędów.

## Zobacz także:
- [Przewodnik po stylu skryptów powłoki Google](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, narzędzie do statycznej analizy skryptów powłoki](https://www.shellcheck.net/)
- [Sztuka wiersza poleceń](https://github.com/jlevy/the-art-of-command-line)
