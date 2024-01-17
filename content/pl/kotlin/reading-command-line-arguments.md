---
title:                "Odczytywanie argumentów z wiersza poleceń"
html_title:           "Kotlin: Odczytywanie argumentów z wiersza poleceń"
simple_title:         "Odczytywanie argumentów z wiersza poleceń"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Odczytywanie argumentów wiersza poleceń jest procesem polegającym na odczytywaniu i wykorzystywaniu informacji przekazywanych do programu podczas jego uruchamiania w wierszu poleceń. Programiści robią to po to, aby móc wpływać na działanie swojego programu w zależności od informacji przekazywanych przez użytkownika.

## Jak to zrobić:
Kotlin dostarcza prosty sposób na odczytanie argumentów wiersza poleceń za pomocą wbudowanej funkcji "args". Poniższy kod pokazuje przykładowe użycie tej funkcji:

```Kotlin
fun main(args: Array<String>) {
    for (arg in args) {
        println(arg)
    }
}

//oczyta i wyświetli wszystkie argumenty przekazane do programu
```

Przykładowy output:
```
argument1 argument2 argument3
```

## Głębsza analiza:
Odczytywanie argumentów wiersza poleceń jest bardzo popularne w zastosowaniach, gdzie konieczne jest dostosowywanie programu do różnych warunków. Alternatywnym sposobem na przekazywanie informacji do programu jest wykorzystanie plików konfiguracyjnych, jednak odczytanie argumentów wiersza poleceń jest szybsze i prostsze w implementacji. W Kotlinie argumenty wiersza poleceń są przechowywane jako tablica ciągów znaków, co umożliwia ich łatwą manipulację.

## Zobacz też:
Dokumentacja Kotlina: https://kotlinlang.org/docs/tutorials/command-line.html