---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Konvertieren eines Strings in Kleinbuchstaben ist ein häufiger Prozess in der Java-Programmierung, bei dem alle Großbuchstaben in einem gegebenen String in Kleinbuchstaben umgewandelt werden. Warum das? Um die Textverarbeitung zu vereinfachen und unnötige Abhängigkeiten von der Groß- und Kleinschreibung zu vermeiden.

## So geht's:

In Java benutzen wir die `toLowerCase()` Methode, um einen String in Kleinbuchstaben zu konvertieren. Hier ist ein kurzes Beispiel.

```Java
public class Main {
    public static void main(String[] args) {
        String text = "Hallo Welt!";
        String lowerCaseText = text.toLowerCase();
        System.out.println(lowerCaseText);
    }
}
```

Dieser Code gibt aus:

```
hallo welt!
```

## Tief Tauchen:

Die `toLowerCase()` Methode gibt es schon seit den Anfängen von Java und sie hat ihren festen Platz in der Standardbibliothek. Alternativ könntest du natürlich durch jeden Buchstaben des Strings iterieren und manuell konvertieren, aber das wäre unnötig kompliziert und ineffizient.

Eine wichtige Sache, die zu beachten ist, ist die Verwendung von `Locale` mit `toLowerCase()`. Die Methode `toLowerCase()` kümmert sich um sprachspezifische Konvertierungsregeln. Wenn du kein `Locale` angibst, verwendet Java das Standardlocale der laufenden JVM, was zu unerwarteten Ergebnissen führen kann. Daher empfiehlt es sich, immer das spezifische Locale zu spezifizieren.

```Java
String text = "İstanbul";
String lowerCaseText = text.toLowerCase(Locale.ENGLISH);
```
Dieser Code wird den Text zu "i̇stanbul" konvertieren, anstelle von "ıstanbul", was in einem türkischen Locale der Fall wäre.

## Sieh auch:

- [Java String toLowerCase() Methode - Javatpoint](https://www.javatpoint.com/java-string-tolowercase)
- [Java String toLowerCase(Locale) Methode - Javatpoint](https://www.javatpoint.com/java-string-tolowercase-locale)
- Das `Locale` Klassen Dokumentation in [Java Standard API (Englisch)](https://docs.oracle.com/javase/7/docs/api/java/util/Locale.html)