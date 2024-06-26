---
date: 2024-01-20 17:56:15.469529-07:00
description: "Jak to zrobi\u0107: Uruchomienie programu z argumentami."
lastmod: '2024-04-05T21:53:36.731510-06:00'
model: gpt-4-1106-preview
summary: Uruchomienie programu z argumentami.
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

## Jak to zrobić:
```java
public class CommandLineReader {
    public static void main(String[] args) {
        if (args.length > 0) {
            System.out.println("Otrzymane argumenty linii poleceń:");
            for(String arg : args) {
                System.out.println(arg);
            }
        } else {
            System.out.println("Brak argumentów linii poleceń.");
        }
    }
}
```
Uruchomienie programu z argumentami:
```
java CommandLineReader jeden dwa trzy
```
Wyjście:
```
Otrzymane argumenty linii poleceń:
jeden
dwa
trzy
```

## Wnikliwe spojrzenie:
Argumenty linii poleceń są używane od zarania informatyki. W Javie dostępne są jako tablica `String`ów przekazywana do metody `main`. Alternatywą jest użycie narzędzi jak JCommander czy Apache Commons CLI, które oferują zaawansowane parsowanie i obsługę opcji. Implementacyjnie, argumenty są przekazywane przez maszynę wirtualną Javy i są dostępne jeszcze przed wykonaniem pierwszego instrukcji programu.

## Zobacz również:
- [Dokumentacja Oracle na temat argumentów linii poleceń](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [JCommander](http://jcommander.org/)
- [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/)
