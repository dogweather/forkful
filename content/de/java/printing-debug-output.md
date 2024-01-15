---
title:                "Debug-Ausgabe drucken"
html_title:           "Java: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debug-Ausgaben werden verwendet, um Informationen während der Programmierung anzuzeigen, um Fehler zu finden und zu beheben. Dies kann dabei helfen, das Debugging-Prozess zu beschleunigen und die Effizienz der Programmierung zu erhöhen.

## Wie man Debug-Ausgaben in Java verwendet

Debug-Ausgaben in Java werden mithilfe der Methode `System.out.println()` erstellt. Diese Methode akzeptiert einen String als Parameter und gibt ihn in der Konsole aus. Zum Beispiel:

```Java
System.out.println("Debug-Ausgabe");
```

Dies würde den Text "Debug-Ausgabe" in der Konsole ausgeben.

Weitere Beispiele:

```Java
int num = 5;
System.out.println(num);
```

Dies würde die Variable `num`, mit dem Wert 5, in der Konsole ausgeben.

```Java
String name = "Max";
System.out.println("Hello " + name);
```

Dies würde den Text "Hello Max" in der Konsole ausgeben.

## Tiefer Einblick

Debug-Ausgaben können auch dazu verwendet werden, den Wert von Variablen während der Programmierung zu überwachen. Zum Beispiel:

```Java
int counter = 0;
while(counter < 10) {
  System.out.println("Der Zählwert beträgt: " + counter);
  counter++;
}
```

Dies würde den aktuellen Wert von `counter` bei jedem Durchlauf der Schleife ausgeben und somit helfen, Fehler in der Logik des Codes zu finden.

Es ist auch möglich, mehrere Variablenwerte in einer Debug-Ausgabe zu kombinieren. Zum Beispiel:

```Java
int width = 10;
int height = 5;
System.out.println("Das Rechteck hat die Maße: " + width + " x " + height);
```

Dies würde den Text "Das Rechteck hat die Maße: 10 x 5" in der Konsole ausgeben.

Zusätzlich zu `System.out.println()` gibt es auch andere Methoden wie `System.out.print()` und `System.out.printf()`, die ebenfalls für Debug-Ausgaben verwendet werden können.

## Siehe auch

- [Java Dokumentation zu System.out](https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html)
- [Tutorial zu Debugging in Java](https://www.baeldung.com/java-debugging)
- [Beispiele für Debug-Ausgaben in Java](https://www.journaldev.com/15041/java-sout-examples)