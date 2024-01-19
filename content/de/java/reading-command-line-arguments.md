---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen von Befehlszeilenargumenten in Java bezieht sich auf die Möglichkeit, Daten von der Kommandozeile an Ihr Programm zu übergeben. Diese Technik ist entscheidend, um die Funktionalität von Programmen bei sich ändernden Anforderungen oder Bedingungen flexibel zu handhaben.

## Anleitung:

Einige nützliche Java-Codebeispiele, wie man Befehlszeilenargumente liest, sind unten beigefügt:

```Java
// Prog.java
public class Prog {
    public static void main(String[] args) {
        for(String str: args) {
            System.out.println(str);
        }
    }
}
```
Führen Sie das obige Programm von der Befehlszeile mit einigen Argumenten aus:

```Shell
java Prog.java Hallo Welt
```
Die Ausgabe wäre:

```Shell
Hallo
Welt
```
## Vertiefung:

Befehlszeilenargumente wurden von Anfang an in der Programmierung verwendet, als Benutzerschnittstellen noch auf der Befehlszeile basierten. Heute ermöglichen sie es unseren Programmen, anpassungsfähig zu sein. Alternativen zum Lesen von Befehlszeilenargumenten könnten die Verwendung von Konfigurationsdateien oder Umgebungsvariablen sein. Beim Lesen der Befehlszeilenargumente in Java fängt der Index sobald das Programm aufgerufen wird bei Null an, `args[0]` ist also das erste Argument nach dem Programmnamen.

## Weiterführende Literatur:
- [Oracle Docs - Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html) 
- [GeeksforGeeks - Command line arguments in Java](https://www.geeksforgeeks.org/command-line-arguments-java/) 
- [StackOverflow - How does the JVM handle command line arguments?](https://stackoverflow.com/questions/890966/what-is-string-args-parameter-in-main-method-java)