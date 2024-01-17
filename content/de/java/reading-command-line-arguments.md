---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Java: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen von Befehlszeilenargumenten ist eine gängige Praxis bei der Java-Programmierung. Es ermöglicht Programmierern, den Code an die Anforderungen des Benutzers anzupassen, indem es ihnen ermöglicht, Eingaben vom Benutzer während der Programmausführung zu akzeptieren.

## Wie geht's:
Um Befehlszeilenargumente in Java zu lesen, können wir die Klasse "Scanner" verwenden. Hier ist ein einfaches Beispiel, um die Funktion zu demonstrieren:

```java
import java.util.Scanner;

public class CommandLineReader {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.println("Gib deinen Namen ein:");
        String name = scanner.nextLine();

        System.out.println("Hallo " + name + ", toll dich kennenzulernen!");
    }
}
```

Wenn wir dieses Programm ausführen und "Java CommandLineReader" als Befehl in der Konsole eingeben, wird das Programm uns auffordern, unseren Namen einzugeben. Aufgrund des Codes, den wir geschrieben haben, wird es unseren eingegebenen Namen in der Konsole ausgeben.

```
Output:
Gib deinen Namen ein:
Max Mustermann
Hallo Max Mustermann, toll dich kennenzulernen!
```

## Tiefentauchen:
Das Parsen von Befehlszeilenargumenten gibt es schon seit den Anfängen der Programmierung. Es ermöglicht Programmierern, die Benutzerinteraktion mit dem Programm zu verbessern und es flexibler zu gestalten. Alternativen zum Lesen von Befehlszeilenargumenten umfassen die Verwendung von Dialogfeldern oder das Lesen von Eingaben aus einer Datei. Die Implementierung von Befehlszeilenargumenten in Java basiert auf der Verwendung der Klasse "Scanner", die die Texteingabe von der Konsole akzeptiert und in verschiedene Datentypen konvertiert.

## Sieh dir auch an:
- [Java 8 Dokumentation zu "Scanner"](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
- [TutorialsPoint-Artikel zu Befehlszeilenargumenten in Java](https://www.tutorialspoint.com/java/java_command_line_arguments.htm)
- [Video-Tutorial zu Befehlszeilenargumenten in Java von "Java From Scratch"](https://www.youtube.com/watch?v=0LWdHKGUQRU)