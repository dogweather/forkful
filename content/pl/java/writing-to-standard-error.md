---
title:                "Java: Pisanie do standardowego błędu"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego wyjścia błędów może być przydatne w wielu sytuacjach, na przykład w celu monitorowania działania programu lub znalezienia błędów, które mogą zostać pominięte w standardowym wyjściu.

## Jak to zrobić

Najprostszym sposobem na wypisanie do standardowego wyjścia błędów w języku Java jest użycie metody `System.err.println()`. Przykładowy kod wyglądałby następująco:

```Java
System.err.println("To jest wiadomość błędu");
```

To polecenie wypisze podaną wiadomość do standardowego wyjścia błędów, które można odczytać w konsoli.

Można również zmienić kolor tekstu wypisywanego do standardowego wyjścia błędów, co może być szczególnie przydatne, jeśli chcesz wyróżnić błędy w konsoli. Przykładowy kod wyglądałby następująco:

```Java
System.err.println("\u001B[31m" + "To jest czerwony tekst błędu");
```

## Deep Dive

Istnieją również inne sposoby na pisanie do standardowego wyjścia błędów w języku Java, takie jak użycie klasy `PrintStream` lub `PrintWriter`. Jednak metoda `System.err.println()` jest najprostszym i najczęściej stosowanym sposobem.

Warto również zaznaczyć, że w standardowej bibliotece języka Java istnieje również klasa `System.console()`, która może być użyta do wypisywania wiadomości do standardowego wyjścia błędów. Jednak nie zawsze jest ona dostępna, zwłaszcza w środowisku, gdzie nie ma dostępu do konsoli.

## Zobacz również

- [Dokumentacja Java: System.err.println](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Dokumentacja Java: System.console](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#console--)
- [Tutorial: Pisanie do standardowego wyjścia błędów](https://www.baeldung.com/java-write-to-system-error)