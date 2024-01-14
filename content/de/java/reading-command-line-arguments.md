---
title:                "Java: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit dem Lesen von Kommandozeilenargumenten beschäftigen? Ganz einfach: Es ermöglicht die Interaktion mit einem Java Programm auf einer tieferen Ebene. Egal ob es darum geht, benutzerdefinierte Einstellungen zu steuern oder bestimmte Aktionen auszuführen, die Verwendung von Kommandozeilenargumenten kann sehr nützlich sein.

## Wie geht's?

Das Lesen von Kommandozeilenargumenten in Java ist eigentlich ganz einfach. Zunächst muss die `main()`-Methode des Programms wie folgt deklariert werden:

```Java
public static void main(String[] args)
```

Die `args` Variable enthält jetzt eine Liste der beim Programmstart übergebenen Argumente. Um auf diese zuzugreifen, können wir beispielsweise die `length`-Methode verwenden, um die Anzahl der Argumente zu ermitteln:

```Java
System.out.println("Anzahl der Argumente: " + args.length);
```

Um ein bestimmtes Argument auszuwählen, können wir die Index-Nummer verwenden. Beachten Sie, dass das erste Argument den Index 0 hat, das zweite den Index 1 usw. Zum Beispiel können wir das erste Argument ausgeben, indem wir `args[0]` verwenden:

```Java
System.out.println("Erstes Argument: " + args[0]);
```

## Tiefere Einblicke

Natürlich gibt es noch viel mehr, was man mit Kommandozeilenargumenten machen kann. Zum Beispiel können wir die Argumente auch in andere Datentypen umwandeln, wie zum Beispiel in Integer oder Double.

Ein weiterer wichtiger Aspekt ist die Verwendung von Optionen oder Flags in den Argumenten. Dies ermöglicht es uns, bestimmte Bedingungen abzufragen und entsprechend im Programmcode zu reagieren. Ein Beispiel hierfür wäre die Verwendung einer Option wie `-verbose`, um detailliertere Ausgaben im Programm einzuschalten.

## Siehe auch

Wenn Sie mehr über die Verwendung von Kommandozeilenargumenten in Java erfahren möchten, können Sie sich die folgenden Ressourcen ansehen:

- [Oracle-Dokumentation zu Kommandozeilenargumenten](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Java - Lesen von Kommandozeilenargumenten](https://www.tutorialspoint.com/java/java_command_line_arguments.htm)
- [Kommandozeilenargumente in Java verwenden](https://www.geeksforgeeks.org/command-line-arguments-in-java/)