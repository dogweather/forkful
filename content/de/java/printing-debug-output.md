---
title:    "Java: Ausgabe von Debugging-Informationen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum

Beim Programmieren gibt es Zeiten, in denen man unsicher ist, was genau im Code passiert. Das ist normal, denn das Debuggen ist ein wichtiger Teil des Entwicklungsprozesses. Um besser zu verstehen, was im Code passiert, kann es hilfreich sein, Debug-Ausgaben zu drucken. In diesem Blog-Beitrag werde ich erklären, warum und wie man Debug-Ausgaben im Java-Code verwenden kann.

## Wie man Debug-Ausgaben druckt

Um Debug-Ausgaben in Java zu drucken, gibt es verschiedene Methoden, aber die einfachste ist die Verwendung der Klasse `System.out`. Diese Klasse hat eine Methode namens `println()`, die eine Zeichenkette (String) auf der Konsole ausgibt. Man kann diese Methode nutzen, um Nachrichten über den Codefluss zu drucken, beispielsweise bei Schleifen oder bedingten Anweisungen. Hier ist ein Beispielcode:

```Java
int x = 10;
System.out.println("Der Wert von x ist: " + x);
```

Die oben genannte Methode gibt die Zeichenkette "Der Wert von x ist: 10" auf der Konsole aus. Man kann auch Variablenwerte direkt in die Methode `println()` einfügen, indem man das Pluszeichen (`+`) verwendet, um die Werte mit der Zeichenkette zu verbinden.

Es gibt auch die Methode `print()`, die ähnlich wie `println()` funktioniert, aber keine neue Zeile nach der Ausgabe hinzufügt. Das kann hilfreich sein, wenn man mehrere Debug-Ausgaben in derselben Zeile haben möchte. Hier ist ein Beispiel:

```Java
int x = 10;
System.out.print("Der Wert von x ist: " + x + ". ");
System.out.print("Das ist ein Debug-Ausgabe.");
```

Dies würde folgende Ausgabe auf der Konsole erzeugen: "Der Wert von x ist: 10. Das ist ein Debug-Ausgabe."

## Tiefergehende Informationen

Während das Drucken von Debug-Ausgaben eine gute Möglichkeit ist, den Codefluss zu verstehen, sollte man darauf achten, dass man nicht zu viele Ausgaben druckt, da dies die Leistung des Programms beeinträchtigen kann. Es ist auch sinnvoll, aussagekräftige Nachrichten zu drucken und nicht einfach nur den Code oder Variablennamen, da dies nicht immer klar ist. Eine gute Praxis ist auch, Debug-Ausgaben in Produktionsumgebungen zu deaktivieren, um die Leistung nicht zu beeinträchtigen.

Eine weitere Möglichkeit, Debug-Ausgaben zu drucken, ist die Verwendung von Logging-Frameworks wie log4j oder slf4j. Diese bieten mehr Kontrolle über die Ausgaben und ermöglichen es, sie in verschiedene Dateien oder mit verschiedenen Ebenen von Detail zu leiten. Sie sind auch nützlich, um Produktions-Logs zu überwachen und Fehler zu finden.

## See Also

- [Java Debugging Tutorial (auf Deutsch)](https://www.tutorials.de/threads/java-fuer-anfaenger-teil-4-debuggen.276054/)
- [Einführung in log4j (auf Deutsch)](https://www.tutorials.de/threads/einf%C3%BChrung-in-log4j.15663/)