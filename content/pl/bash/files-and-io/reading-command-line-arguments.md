---
title:                "Odczytywanie argumentów linii poleceń"
aliases: - /pl/bash/reading-command-line-arguments.md
date:                  2024-01-20T17:55:28.350388-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie argumentów linii poleceń"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Czytanie argumentów z linii poleceń pozwala skryptom Bash przyjmować dane wejściowe podczas uruchamiania. Używamy tego, bo to elastyczny sposób na dostosowywanie zachowania skryptów na podstawie użytkownika lub kontekstu uruchomienia.

## Jak to zrobić:
```Bash
#!/bin/bash
echo "Pierwszy argument: $1"
echo "Drugi argument: $2"
echo "Wszystkie argumenty: $@"
echo "Ilość argumentów: $#"
```
Uruchomienie skryptu:
```Bash
$ ./skrypt.sh Arg1 Arg2 Arg3
Pierwszy argument: Arg1
Drugi argument: Arg2
Wszystkie argumenty: Arg1 Arg2 Arg3
Ilość argumentów: 3
```

## Pogłębienie:
Kiedyś pisaliśmy skrypty bez możliwości łatwego przekazywania opcji. Teraz, z argumentami, możemy tworzyć bardziej elastyczne i interaktywne skrypty. Alternatywy jak `getopts` czy `optarg` pozwalają na bardziej zaawansowane przetwarzanie opcji, np. flag (`-h`) czy długa opcja (`--help`). W Bashu, argumenty są dostępne poprzez specjalne zmienne `$1`, `$2`, itd., aż do `$9` – dla pierwszych dziewięciu argumentów, `$0` jest nazwą skryptu, `$@` zawiera wszystkie argumenty, a `$#` mówi, ile ich jest.

## Zobacz też:
- [Bash Scripting Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)
- [Advanced Bash-Scripting Guide](http://tldp.org/LDP/abs/html/)
