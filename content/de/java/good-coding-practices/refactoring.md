---
date: 2024-01-26 01:39:07.162169-07:00
description: 'Wie geht das: Nehmen wir eine einfache Java-Klasse, die aufgrund ihrer
  schlechten Organisation und mangelnden Klarheit nach einem Refactoring verlangt.'
lastmod: '2024-03-13T22:44:53.770868-06:00'
model: gpt-4-0125-preview
summary: Nehmen wir eine einfache Java-Klasse, die aufgrund ihrer schlechten Organisation
  und mangelnden Klarheit nach einem Refactoring verlangt.
title: Refactoring
weight: 19
---

## Wie geht das:
Nehmen wir eine einfache Java-Klasse, die aufgrund ihrer schlechten Organisation und mangelnden Klarheit nach einem Refactoring verlangt.

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // Andere Operationen...
    }
}
```

Nach dem Refactoring haben wir:

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // Andere Operationen...
}
```

Durch das Refactoring haben wir die Methodennamen und -parameter für eine bessere Lesbarkeit verbessert und die Notwendigkeit eines bedingten Verzweigungs innerhalb einer einzigen Methode beseitigt. Jede Operation drückt nun eindeutig ihren Zweck aus.

## Tiefergehende Betrachtung:
Refactoring hat seine Wurzeln in der Smalltalk-Community, mit ihrem Schwerpunkt auf Code-Lesbarkeit und objektorientiertem Design, aber es nahm wirklich in der Java-Welt Ende der 90er und Anfang der 2000er Jahre an Fahrt auf, insbesondere nach der Veröffentlichung von Martin Fowlers bahnbrechendem Buch „Refactoring: Improving the Design of Existing Code.“

Es gibt Alternativen zum Refactoring, wie das Neuschreiben von Code von Grund auf. Refactoring wird jedoch oft bevorzugt, weil es inkrementelle Änderungen beinhaltet, die die Funktionalität der Anwendung nicht unterbrechen.

Implementierungsdetails beim Refactoring in Java (oder jeder anderen Programmiersprache) drehen sich um das Verständnis von Codegerüchen – Indikatoren für tieferliegende Probleme im Code. Einige Gerüche umfassen lange Methoden, große Klassen, doppelten Code und übermäßigen Gebrauch von Primitiven. Durch die Anwendung von Refactoring-Mustern wie Methode extrahieren, Methode verschieben oder Temp durch Abfrage ersetzen, können Entwickler diese Gerüche systematisch angehen, während sie jederzeit sicherstellen, dass der Code funktionsfähig bleibt.

Automatisierte Tools, wie die Refactoring-Unterstützung von IntelliJ IDEA oder Plugins für Eclipse, können den Prozess unterstützen, indem sie Refactorings wie das Umbenennen von Variablen, Methoden und Klassen, das Extrahieren von Methoden oder Variablen sowie das Verschieben von Methoden oder Klassen in andere Pakete oder Namensräume automatisieren.

## Siehe auch:
- Martin Fowlers „Refactoring: Improving the Design of Existing Code“: https://martinfowler.com/books/refactoring.html
- Refactoring-Techniken auf Refactoring.Guru: https://refactoring.guru/refactoring/techniques
- Automatisiertes Refactoring in Eclipse: https://www.eclipse.org/eclipse/news/4.18/jdt.php
- Refactoring-Funktionen von IntelliJ IDEA: https://www.jetbrains.com/idea/features/refactoring.html

Jede dieser Ressourcen bietet entweder eine Grundlage zum Verständnis der Prinzipien des Refactorings oder Werkzeuge, die verwendet werden können, um diese Prinzipien in die Praxis umzusetzen.
