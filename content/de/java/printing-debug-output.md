---
title:                "Java: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Das Drucken von Debug-Ausgaben ist eine nützliche Praxis bei der Programmierung, um Fehler zu finden und zu beheben. Durch das Ausgeben von Informationen während der Ausführung des Programms können Entwickler den Zustand und die Werte von Variablen überprüfen und somit die zugrundeliegenden Probleme identifizieren.

## Wie

Um Debug-Ausgaben in Java zu erstellen, verwenden wir die Methode "System.out.println()", die eine Zeichenkette oder den Wert einer Variablen auf der Konsole ausgibt. Im folgenden Beispiel wird eine Schleife verwendet, um die Zahlen von 1 bis 10 auszugeben:

```Java
for(int i = 1; i <= 10; i++) {
    System.out.println(i);
}
```

Die Ausgabe sieht folgendermaßen aus:

```
1
2
3
4
5
6
7
8
9
10
```

Auf diese Weise können wir den Wert von "i" überprüfen, um sicherzustellen, dass die Schleife korrekt funktioniert.

## Deep Dive

Es gibt verschiedene Möglichkeiten, um Debug-Ausgaben in Java zu gestalten, einschließlich der Verwendung von Formatierung für bessere Lesbarkeit und der Verwendung von Logging-Frameworks wie Log4j. Auch das Debuggen von Multithreading- und asynchronen Programmen erfordert etwas fortgeschrittenere Techniken wie das Verwenden von Breakpoints und das Erstellen von Ausgaben innerhalb von Threads.

Es ist wichtig zu beachten, dass Debug-Ausgaben nicht in der Produktionsumgebung verwendet werden sollten, da sie die Leistung beeinträchtigen und potenziell sensible Informationen offenlegen können.

## Siehe auch

- [Debugging in Java](https://www.javatpoint.com/java-debugging)
- [Log4j Dokumentation](https://logging.apache.org/log4j/2.x/manual/index.html)
- [Java Multithreading](https://www.edureka.co/blog/introduction-to-multithreading-in-java/)