---
title:                "Ausgabe von Fehlerbehebung drucken"
html_title:           "Java: Ausgabe von Fehlerbehebung drucken"
simple_title:         "Ausgabe von Fehlerbehebung drucken"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Debug-Ausgabe oder einfach nur "printn" ist eine Technik, die Programmierer verwenden, um während der Entwicklung zusätzliche Informationen aus ihrem Code zu extrahieren. Durch das Hinzufügen von printn-Anweisungen können Programmierer sehen, was ihr Code tatsächlich tut und Fehler besser verstehen.

## Wie man:

Hier ein einfaches Beispiel, um zu zeigen, wie Debug-Ausgabe in Java verwendet wird:

```Java
int a = 5, b = 10, c;
c = a + b;
System.out.println("c = " + c);
```

Die Ausgabe des Codes wird "c = 15" sein. Durch die Verwendung von Debug-Ausgaben im Code können Programmierer sehen, wie der Wert von "c" in jeder Iteration des Programms verändert wird. Dies kann helfen, Probleme zu lokalisieren und zu beheben.

## Tief getaucht:

Historischer Kontext: Debug-Ausgabe ist eine der ältesten und grundlegendsten Techniken zur Fehlerbehebung in der Programmierung. Sie wurde bereits in den frühen Tagen der Computerentwicklung verwendet und ist auch heute noch ein wichtiger Bestandteil der Entwicklung.

Alternativen: Obwohl printn die gebräuchlichste Methode ist, um Debugging-Ausgaben in Java zu erzeugen, gibt es andere Alternativen wie das Logging-Framework log4j oder die Java Debugger-Schnittstelle.

Implementierungsdetails: In Java können Debug-Ausgaben einfach durch die Verwendung der System-Klasse und des printn-Befehls implementiert werden. Es ist auch wichtig zu beachten, dass Debug-Ausgaben aus Performance-Gründen oft deaktiviert werden sollten, wenn die Anwendung in Produktion läuft.

## Siehe auch:

[Offizielle Java-Dokumentation über Debugging-Ausgaben](https://docs.oracle.com/javase/tutorial/essential/exceptions/debug.html)

[Java Logging-Framework log4j](https://logging.apache.org/log4j/2.x/)

[Java Debugger-Schnittstelle](https://docs.oracle.com/javase/8/docs/technotes/guides/jpda/)