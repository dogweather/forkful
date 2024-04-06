---
date: 2024-01-20 17:55:28.350388-07:00
description: "Jak to zrobi\u0107: Kiedy\u015B pisali\u015Bmy skrypty bez mo\u017C\
  liwo\u015Bci \u0142atwego przekazywania opcji. Teraz, z argumentami, mo\u017Cemy\
  \ tworzy\u0107 bardziej elastyczne i\u2026"
lastmod: '2024-04-05T22:50:49.926380-06:00'
model: gpt-4-1106-preview
summary: "Kiedy\u015B pisali\u015Bmy skrypty bez mo\u017Cliwo\u015Bci \u0142atwego\
  \ przekazywania opcji."
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

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
