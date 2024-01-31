---
title:                "Odczytywanie argumentów linii poleceń"
date:                  2024-01-20T17:56:20.422692-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie argumentów linii poleceń"

category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Czytanie argumentów linii poleceń to po prostu odbieranie danych, które użytkownik wpisuje razem z nazwą uruchamianego programu. Programiści korzystają z tego, żeby umożliwić użytkownikom wpływanie na działanie programu bez zmian w kodzie.

## How to:
```
// Kotlin

fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        args.forEach { arg ->
            println("Przekazany argument: $arg")
        }
    } else {
        println("Nie podano argumentów.")
    }
}

```
**Sample Output:**
```
> kotlinc Main.kt -include-runtime -d Main.jar
> java -jar Main.jar Witaj Świecie

Przekazany argument: Witaj
Przekazany argument: Świecie
```

## Deep Dive
Argumenty linii poleceń są tak stare jak same wczesne komputery. Dawały one możliwość interakcji z programem bez potrzeby tworzenia interfejsu użytkownika. W Kotlinie, dostęp do nich jest poprzez `args: Array<String>` w funkcji `main`. Alternatywą może być użycie zewnętrznych bibliotek, jak `Kotlinx-cli`, które oferują rozszerzone możliwości, np. parsowanie czy validację argumentów. Ważne: Kotlin uruchamiany jest na JVM, więc argumenty są przetwarzane identycznie jak w Javie.

## See Also
- [Kotlin Documentation](https://kotlinlang.org/docs/command-line.html)
- [GitHub - kotlinx-cli](https://github.com/Kotlin/kotlinx-cli)
