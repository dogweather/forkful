---
date: 2024-01-20 17:56:20.422692-07:00
description: "How to: Argumenty linii polece\u0144 s\u0105 tak stare jak same wczesne\
  \ komputery. Dawa\u0142y one mo\u017Cliwo\u015B\u0107 interakcji z programem bez\
  \ potrzeby tworzenia interfejsu\u2026"
lastmod: '2024-04-05T22:50:49.698540-06:00'
model: gpt-4-1106-preview
summary: "Argumenty linii polece\u0144 s\u0105 tak stare jak same wczesne komputery."
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

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
