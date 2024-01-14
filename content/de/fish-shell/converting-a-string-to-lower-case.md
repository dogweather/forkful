---
title:    "Fish Shell: Ein String in Kleinbuchstaben umwandeln"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Die Konvertierung von Zeichenketten in Kleinbuchstaben kann in der Programmierung oft nützlich sein, um zum Beispiel Vergleiche zwischen Zeichenketten zu erleichtern. In diesem Blog-Beitrag werden wir uns ansehen, wie dies mit der Fish Shell möglich ist.

## Wie es geht

Die Fish Shell bietet eine eingebaute Funktion namens "tolower", die eine Zeichenkette in Kleinbuchstaben konvertiert. Hier ist ein Beispiel, wie man diese Funktion verwenden kann:

```Fish Shell
set string "HALLO WELT"
echo $string | tolower
```

Dies wird die Ausgabe "hallo welt" erzeugen. Wie man sehen kann, wird die Funktion einfach an die Zeichenkette angehängt, die man konvertieren möchte.

## Tiefergehende Analyse

Die Funktion "tolower" in der Fish Shell verwendet die Standardfunktion "tr" aus dem UNIX-Betriebssystem, um die Zeichenkettenkonvertierung durchzuführen. Der Funktionsaufruf sieht also in Wirklichkeit so aus:

```Fish Shell
set string "HALLO WELT"
echo $string | tr '[:upper:]' '[:lower:]'
```

In diesem Fall ist "[:upper:]" eine Klassenbezeichnung für alle Großbuchstaben und "[:lower:]" für alle Kleinbuchstaben. Diese Methode kann auch auf andere Zeichenketten angewendet werden, zum Beispiel zum Konvertieren von allen Buchstaben in Großbuchstaben mit "[:lower:]" und "[:upper:]".

## Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/)
- [Fish Shell Github Repository](https://github.com/fish-shell/fish-shell)
- [Tutorial zu Zeichenketten in der Programmierung](https://www.geeksforgeeks.org/string-manipulation-in-c-without-using-inbuilt-library-functions/)