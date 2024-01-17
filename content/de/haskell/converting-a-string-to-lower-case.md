---
title:                "Umwandeln eines Strings in Kleinbuchstaben"
html_title:           "Haskell: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Konvertierung eines Strings in Kleinbuchstaben ist ein häufig verwendetes Konzept in der Programmierung. Dabei handelt es sich um die Umwandlung eines Strings, also einer Zeichenkette, in eine neue Zeichenkette, in der alle Buchstaben klein geschrieben sind. Programmierer verwenden diese Methode, um sicherzustellen, dass Benutzereingaben konsistent verarbeitet werden und um Vergleiche zwischen Strings einfacher zu gestalten.

## Wie geht's?

Um einen String in Kleinbuchstaben umzuwandeln, können wir die Funktion `toLower` aus dem Modul `Data.Char` verwenden.

```Haskell
import Data.Char

main = do
    let input = "HELLO WORLD"
    let output = map toLower input
    print output
```

Die Funktion `toLower` nimmt einen Charakter als Argument und gibt den entsprechenden Kleinbuchstaben zurück. Durch die Anwendung der `map` Funktion auf unseren String können wir jeden Buchstaben einzeln in Kleinbuchstaben umwandeln. Das Ergebnis wird dann in der Variablen `output` gespeichert und mit `print` ausgegeben.

Der Output dieses Programms wäre `hello world`.

## Tiefer eintauchen

Die Notwendigkeit, Strings in Kleinbuchstaben umzuwandeln, ergibt sich aus der Funktionsweise von Computern. In der ASCII-Zeichencodierung (die immer noch in den meisten Programmiersprachen verwendet wird) sind Groß- und Kleinbuchstaben unterschiedliche Zeichen und haben daher unterschiedliche numerische Codes. Wenn wir also den String `"Hello"` mit dem String `"hello"` vergleichen wollen, erhalten wir ein anderes Ergebnis, da die Groß- und Kleinbuchstaben in ASCII unterschiedliche Zahlenwerte haben. Durch die Konvertierung in Kleinbuchstaben können wir sicherstellen, dass beide Strings äquivalent sind.

Alternativ zur Verwendung der `toLower` Funktion könnte man auch die `map` Funktion mit `toLower . ord` kombinieren. Dabei wird der Ordinalwert eines Zeichens zuerst in Kleinbuchstaben umgewandelt und dann wieder in ein Zeichen zurückkonvertiert. Dieser Ansatz ist jedoch weniger effizient.

Die Implementation der `toLower` Funktion ist abhängig von der verwendeten Compiler-Implementierung. In der gängigsten Implementierung von Haskell, GHC, wird die Funktion durch Aufruf von C-Bibliothekscode implementiert, der wiederum die entsprechende Funktion des Betriebssystems verwendet.

## Siehe auch

- [Haskells `toLower` Funktion Dokumentation](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html#v:toLower)
- [ASCII Zeichencodierung](https://de.wikipedia.org/wiki/American_Standard_Code_for_Information_Interchange)
- [Haskell GHC Compiler](https://www.haskell.org/ghc/)