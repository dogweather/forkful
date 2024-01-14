---
title:                "Java: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren kommt es oft vor, dass Fehler auftreten. Diese können verschiedene Ursachen haben und sind manchmal schwer zu finden. Das Schreiben zu Standardfehler (STDERR) kann dabei helfen, diese Fehler schneller und effizienter zu erkennen.

## Wie geht man vor?

Um zu Standardfehler zu schreiben, gibt es in Java die Methode `System.err.println()`. Diese Methode nimmt einen String als Parameter und gibt ihn auf dem Standardfehlerkanal aus. Zum Beispiel:

```Java
System.err.println("Fehler: Division durch Null");
```

Dies würde den String "Fehler: Division durch Null" auf dem Standardfehlerkanal ausgeben.

## Tiefergehende Informationen

Beim Schreiben zu Standardfehler sollte man beachten, dass dies auf dem gleichen Kanal wie der Standardausgabekanal (STDOUT) geschieht. Das bedeutet, dass die Reihenfolge der Ausgaben nicht garantiert ist und es zu Interferenzen kommen kann. 

Außerdem ist es wichtig zu wissen, dass es auch möglich ist, zu Standardfehler zu lesen. Dafür gibt es die Methode `System.err.read()`, welche einen einzelnen Character aus dem Standardfehlerkanal liest. 

## Siehe auch

- [Standardfehlerkanal in Java](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#err)
- [Fehlerbehandlung in Java](https://www.tutorialspoint.com/java/java_exceptions.htm)
- [Debugging in Java](https://www.javatpoint.com/debugging-in-java)