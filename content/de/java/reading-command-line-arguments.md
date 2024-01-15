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

## Warum

Warum sollte man sich mit dem Lesen von Befehlszeilenargumenten beschäftigen? Nun, wenn du als Java-Entwickler arbeitest, kann es eine sehr nützliche Fähigkeit sein, da viele Anwendungen Befehlszeilenargumente zur Konfiguration verwenden. Es ermöglicht dir auch, deine Programme an verschiedene Einstellungen anzupassen, ohne den Code ändern zu müssen.

## Wie geht es

Um Befehlszeilenargumente in deinem Java-Programm zu lesen, gibt es einige Schritte, die du befolgen musst:

1. Definiere die Main-Methode deines Programms mit dem Argument *String args[]* als Parameter.
2. Nutze die *args* Variable, um auf die übergebenen Befehlszeilenargumente zuzugreifen.

Um dir ein besseres Verständnis zu geben, hier ist ein Beispielcode, der den Namen des Benutzers als Befehlszeilenargument erwartet und dann eine personalisierte Begrüßung ausgibt:

```Java
public class Begrüßung {
    public static void main(String args[]) {
        System.out.println("Hallo " + args[0] + "! Willkommen zu unserem Programm.");
    }
}
```

Wenn du dieses Programm mit dem Befehl `java Begrüßung Max` ausführst, wird es folgende Ausgabe erzeugen:

```
Hallo Max! Willkommen zu unserem Programm.
```

## Deep Dive

Es gibt einige Dinge, die du bei der Arbeit mit Befehlszeilenargumenten beachten solltest:

- Befehlszeilenargumente werden in Form von Strings an dein Programm übergeben, also musst du sie entsprechend konvertieren, wenn du sie als andere Datentypen nutzen möchtest.
- Du kannst mehrere Befehlszeilenargumente an dein Programm übergeben, indem du sie bei der Ausführung mit einem Leerzeichen trennst. Zum Beispiel `java Rechner 5 10` würde das Programm mit zwei Befehlszeilenargumenten ausführen: 5 und 10.
- Du kannst auch Default-Werte festlegen, falls keine Befehlszeilenargumente übergeben werden, indem du eine Bedingung einbaust, die überprüft, ob *args* leer ist.

Für eine tiefergehende Auseinandersetzung mit Befehlszeilenargumenten kannst du auch die Java-Standardbibliotheksklasse `java.util.Scanner` nutzen, um die Eingabe des Benutzers aus der Konsole zu lesen.

## Siehe auch

- [Java Dokumentation: Befehlszeilenargumente](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Java Dokumentation: java.util.Scanner](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)