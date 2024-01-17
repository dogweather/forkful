---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Haskell: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Was ist das Konvertieren eines Datums in einen String und warum machen Programmierer das?

Das Konvertieren eines Datums in einen String bezieht sich auf die Umwandlung eines Datums, das in einem bestimmten Format gespeichert ist, in einen lesbaren Text. Dies kann nützlich sein, wenn Benutzer einen bestimmten Textstil bevorzugen oder ein Datum in einer bestimmten Sprache angezeigt werden soll. Programmierer führen diese Konvertierung durch, um die Lesbarkeit von Daten zu verbessern und sie für Benutzer oder andere Programmierer zugänglicher zu machen.

Wie funktioniert es?

Die Konvertierung eines Datums in einen String kann mit der `show` Funktion in Haskell erreicht werden. Diese Funktion nimmt ein Datum als Eingabe und gibt einen lesbaren String aus, der das Datum darstellt. Hier ein Beispiel:

```Haskell
show (2019, 10, 20) -- Output: "(2019,10,20)"
```

In diesem Beispiel wird ein Tuple mit einem Datum als Eingabe verwendet. Jedoch kann die `show` Funktion auch mit anderen Datentypen wie `Int`, `Float` oder `Bool` verwendet werden.

Als Alternative zur `show` Funktion gibt es auch die `formatTime` Funktion, die es ermöglicht, ein Datum in einem benutzerdefinierten Format auszugeben. Hier ein Beispiel:

```Haskell
import Data.Time.Format (formatTime, defaultTimeLocale)

formatTime defaultTimeLocale "%d.%m.%Y" (2019, 10, 20) -- Output: "20.10.2019"
```

Tiefer eintauchen

Die `show` Funktion hat eine lange Geschichte in der Programmiersprache Haskell. Sie war Teil des ursprünglichen Standard Packages und wurde seitdem in verschiedene andere Packages importiert. Sie ist weiterhin eine beliebte Wahl für die Konvertierung von Datumsangaben in Strings aufgrund ihrer Einfachheit und Effektivität.

Alternativ zur Verwendung von `show` und `formatTime`, können Programmierer auch eine benutzerdefinierte Funktion schreiben, um ein Datum in einen String zu konvertieren. Dies erfordert jedoch mehr Aufwand und Wissen über die internen Funktionen von Haskell.

Schau dir auch an

- [Haskell.org - `show` Funktion](https://wiki.haskell.org/Show_instance)
- [Haskell.org - `formatTime` Funktion](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#g:5)

Übersetzt aus dem Englischen von Gaelle McNeil