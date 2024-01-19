---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Ein Haskell Handbuch: Finden Sie die Länge eines Strings

## Was und Warum?

Die Länge eines Strings zu finden bedeutet, die Anzahl der Zeichen (einschließlich Leerzeichen) in einer Zeichenkette zu zählen. Programmierer tun dies oft, um Speicher zu verwalten oder beim Iterieren durch Strings zu helfen.

## Wie wird's gemacht?

In Haskell ist die Länge eines Strings einfach mit der eingebauten Funktion `length` zu bestimmen. Hier ein einfaches Beispiel:

```haskell
main = do
    let meinString = "Hallo, Welt!"
    putStrLn ("Die Länge des Strings ist: " ++ show (length meinString))
```

Wenn Sie dieses Programm ausführen, liefert es folgende Ausgabe:
```
Die Länge des Strings ist: 13
```

## Tiefer eintauchen

Historisch gesehen, hat Haskell seine eingebaute Funktion `length` seit seiner allerersten Version. Es gibt jedoch Alternativen zum Finden der Länge eines Strings. Zum Beispiel, statt `length` kann die Funktion `foldl'` verwendet werden:

```haskell
import Data.List

main = do
    let len = foldl' (\n _ -> n + 1) 0 "Hallo, Welt!"
    print len
```

Das liefert das gleiche Ergebnis, kann aber in Behandlung großer Datensätze effizienter sein, weil `foldl'` nicht die gesamte Liste im Speicher halten muss.

Bedenken Sie aber, dass `length` und `foldl'` nicht mit Endloslisten funktionieren werden.

## Siehe auch

- Offizielle Dokumentation für `length`: <https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:length>

- Einführung in Haskell: <https://www.haskell.org/documentation/>

- Haskell Wiki-Artikel über Faltungen: <https://wiki.haskell.org/Fold>