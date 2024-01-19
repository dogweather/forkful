---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie argumentów z linii poleceń to proces pobierania danych wprowadzonych przy uruchomieniu skryptu. Programiści robią to, aby umożliwić użytkownikom dostosowywanie działania programu zamiast tworzyć stałe skrypty.

## Jak to zrobić:

Here's how you read command line arguments in a bash script:

```Bash
#!/bin/bash
echo "Pierwszy argument: $1"
echo "Drugi argument: $2"
echo "Wszystkie argumenty: $@"
```
Przykładowe wyjście dla `./myscript.sh argument1 argument2`:

```Bash
Pierwszy argument: argument1
Drugi argument: argument2
Wszystkie argumenty: argument1 argument2
```

## Pogłębione informacje

1) Kontekst historyczny: Bash (Bourne Again Shell) pojawił się w 1989 roku jako ulepszona wersja Bourne Shell (sh). Zasady przekazywania argumentów z linii poleceń zostały z niego zaczerpnięte.

2) Alternatywy: Inne shelle jak zsh czy fish mają podobne mechanizmy obsługi argumentów, ale mogą zawierać dodatkowe funkcje.

3) Szczegóły implementacji: Argumenty z linii poleceń są dostępne jako specjalne zmienne: $1, $2, ..., $n, gdzie n to numer argumentu. $@ to specjalna zmienna zawierająca wszystkie argumenty.

## Zobacz również

- Przewodnik Bash dla początkujących: https://www.gnu.org/software/bash/manual/bash.html
- Porównanie różnych shelli: https://www.tecmint.com/different-linux-shells/
- Wyjaśnienie o zmiennej $@: https://www.gnu.org/software/bash/manual/html_node/Special-Parameters.html