---
title:                "Bash: Odczytywanie argumentów wiersza poleceń"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego
Programowanie w Bashu może wydawać się na pierwszy rzut oka przerażające, ale znajomość jego podstaw jest niezwykle przydatna dla każdego, kto zajmuje się tworzeniem skryptów lub automatyzacją zadań administracyjnych na swoim systemie operacyjnym. Jednym z najważniejszych aspektów programowania w Bashu jest umiejętność czytania argumentów z linii poleceń, co pozwala na łatwiejsze i szybsze przekazywanie danych do naszych skryptów.

## Jak
Aby zacząć czytać argumenty z linii poleceń w skrypcie Bash, musimy najpierw ustalić, które argumenty chcemy przekazać. Możemy to zrobić za pomocą zmiennych specjalnych, takich jak `$1`, `$2`, itp., które odpowiadają kolejnym argumentom przekazanym do skryptu. Przykładowo, jeśli nasz skrypt to `hello.sh`, a chcemy przekazać mu dwa argumenty (np. `John` i `Doe`), to możemy użyć ich w następujący sposób:

```
Bash hello.sh John Doe
```

Aby teraz odczytać te argumenty w skrypcie, użyjemy zmiennych specjalnych:

```Bash
#!/bin/bash
echo "Witaj, $1 $2!" #wypisze "Witaj, John Doe!"
```

Możemy również wykorzystać pętlę `for` do odczytania wszystkich przekazanych argumentów, na przykład:

```Bash
#!/bin/bash
for arg in "$@" #pętla wykonuje się dla każdego argumentu przekazanego do skryptu
do
    echo "$arg" #wypisze "John", a potem "Doe" w kolejnych iteracjach pętli
done
```

## Deep Dive
Istnieją także inne metody odczytywania argumentów z linii poleceń w Bashu, takie jak flagi czy opcje. Flagi to pojedyncze znaki poprzedzane znakiem minusa (`-`), które możemy przypisać do zmiennych, np. `a=0` lub `b=true`. Opcje to ciągi znaków poprzedzane podwójnym minusem (`--`) i przekazane w postaci `nazwa=wartość`. W naszym skrypcie możemy użyć programu `getopts`, aby łatwiej przekazać i odczytać flagi i opcje. Możemy też skorzystać z biblioteki `getopt`, która jest bardziej rozbudowana i daje więcej możliwości w obsłudze argumentów.

## Zobacz także
- [Bash Guide for Beginners](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash Arguments, Options, Flags and Commands](https://ryanstutorials.net/bash-scripting-tutorial/bash-arguments.php)
- [getopts man page](https://www.man7.org/linux/man-pages/man1/getopts.1.html)