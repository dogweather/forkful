---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Czytanie argumentów linii poleceń to proces wprowadzania danych bezpośrednio ze środowiska uruchomieniowego, zamiast korzystać z GUI lub pliku wejściowego. Programiści robią to, aby skrypt był bardziej elastyczny i można go było dostosować poprzez przekazanie różnych parametrów bezpośrednio podczas wywołania.

## Jak to zrobić:

W Kotlinie, argumenty linii poleceń są przechowywane w tablicy argumentów, która jest przekazywana do głównej funkcji. Oto przykład:

```Kotlin
fun main(args: Array<String>){
    for (arg in args) {
        println(arg)
    }
}
```

Gdy uruchomisz ten program, argumenty przekazane z linii poleceń zostaną wyświetlone. Na przykład, gdy uruchomisz `kotlin MainKt arg1 arg2 arg3`, to zobaczysz:

```
arg1
arg2
arg3
```

## Deep Dive:

Historia: Wcześniejsze języki programowania, takie jak C i Python, również obsługiwały argumenty linii poleceń, co uczyniło tę funkcję standardem dla wielu języków, w tym Kotlin.

Alternatywy: Inne możliwości wprowadzania danych w Kotlinie obejmują wczytywanie danych z pliku lub z interfejsu użytkownika.

Szczegóły implementacji: Kotlin obsługuje domyślnie argumenty linii poleceń poprzez funkcję `main()`. Argumenty są przechowywane jako tablica stringów (`Array<String>`), co oznacza, że ​​wszystko, co jest przekazywane z linii poleceń, jest traktowane jako string. 

## Zobacz też:

1. Dokumentacja Kotlina: [Praca z tablicami w Kotlinie](https://kotlinlang.org/docs/arrays.html)
2. Jak przekazywać argumenty do programów w innych językach: [Pasowanie argumentów w Pythonie](https://docs.python.org/3/tutorial/inputoutput.html), [Pasowanie argumentów w C](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)