---
date: 2024-01-26 01:11:06.658918-07:00
description: "Wie geht das: Hier ist ein klassisches Beispiel - eine Funktion zur\
  \ Berechnung der Fakult\xE4t einer Zahl."
lastmod: '2024-03-13T22:44:53.768241-06:00'
model: gpt-4-1106-preview
summary: "Hier ist ein klassisches Beispiel - eine Funktion zur Berechnung der Fakult\xE4\
  t einer Zahl."
title: Code in Funktionen organisieren
weight: 18
---

## Wie geht das:
Hier ist ein klassisches Beispiel - eine Funktion zur Berechnung der Fakultät einer Zahl.

```java
public class MathUtils {

    public static void main(String[] args) {
        int number = 5;
        int result = factorial(number);
        System.out.println("Fakultät von " + number + " ist: " + result);
    }
    
    public static int factorial(int n) {
        if (n <= 1) {
            return 1;
        }
        return n * factorial(n - 1);
    }
}
```

Die Ausgabe wäre:
```
Fakultät von 5 ist: 120
```

## Tiefergehende Betrachtung
Bevor Funktionen üblich waren, wurde Code in monolithische Blöcke gequetscht, was das Debuggen wie die Suche nach einer Nadel im Heuhaufen machte. Jetzt hilft die Kapselung von Funktionalitäten in Funktionen dabei, Probleme schnell zu isolieren. Alternativen beinhalten Lambda-Ausdrücke in Java oder Methoden in der objektorientierten Programmierung, die beide ähnliche Zwecke erfüllen. Wenn Sie eine Funktion schreiben, denken Sie daran: (1) Jede Funktion sollte eine einzelne Verantwortlichkeit haben und (2) der Name der Funktion sollte ihren Zweck klar beschreiben.

## Siehe auch
Für mehr über die Organisation von Code:
- Clean Code von Robert C. Martin
- Refactoring: Die Neugestaltung von bestehendem Code von Martin Fowler
- [Oracle Java-Dokumentation zum Definieren von Methoden](https://docs.oracle.com/javase/tutorial/java/javaOO/methods.html)
