---
title:                "Schreiben auf die Standardfehlerausgabe."
html_title:           "Java: Schreiben auf die Standardfehlerausgabe."
simple_title:         "Schreiben auf die Standardfehlerausgabe."
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Schreiben von Programmen kommt es oft vor, dass man sich mit verschiedenen Problemen und Fehlern auseinandersetzen muss. In solchen Fällen kann es hilfreich sein, die Fehlermeldungen oder wichtige Informationen nicht auf der normalen Ausgabe (standard output), sondern auf dem sogenannten Standardfehler (standard error) auszugeben. Dadurch können Fehler schneller erkannt und behoben werden.

## Wie geht's?
Um etwas auf dem Standardfehler auszugeben, benutzt man in Java die Methode `System.err.println()`. Dabei wird die Nachricht, die man auf dem Fehlerausgabekanal ausgeben möchte, als Parameter in die Methode gegeben. Hier ein Beispiel:

```Java
System.err.println("Es ist ein Fehler aufgetreten!");
```

Die Ausgabe sieht dann in der Konsole etwa so aus:

```none
Es ist ein Fehler aufgetreten!
```

## Tiefere Einblicke
Die Idee, wichtige Informationen auf dem Standardfehler auszugeben, kommt aus der Unix-Welt. Dort gibt es verschiedene Ausgabe-Kanäle, die für unterschiedliche Zwecke genutzt werden. Während die normale Ausgabe nur für normale Ausgaben gedacht ist, kann der Standardfehler für Fehlermeldungen oder wichtige Hinweise genutzt werden.

Als Alternative zur `System.err` Methode kann auch die `System.out` Methode genutzt werden, um Meldungen auf dem Standardfehler auszugeben. Allerdings wird diese Methode auch für die normale Ausgabe verwendet, weshalb es besser ist, die `System.err` Methode zu nutzen.

## Siehe auch
- [Java Dokumentation zur System-Klasse](https://docs.oracle.com/javase/10/docs/api/java/lang/System.html)
- [Wikipedia-Artikel über Standard Streams](https://en.wikipedia.org/wiki/Standard_streams#Standard_error)