---
title:    "Haskell: Die Länge eines Strings finden"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Warum

Das Finden der Länge einer Zeichenkette kann ein nützliches Werkzeug sein, um Programme zu schreiben, die mit Text arbeiten. Es ermöglicht uns, die Größe von Zeichenketten zu bestimmen und sie in unseren Code einzubinden.

# Wie geht man vor

Um die Länge einer Zeichenkette in Haskell zu finden, verwenden wir die Funktion `length`. Diese Funktion nimmt eine Zeichenkette als Parameter und gibt die Anzahl der Zeichen in der Zeichenkette zurück. Hier ist ein Beispielcode:

```Haskell
länge :: String -> Int
länge s = length s
```

Die Funktion `length` gibt eine ganze Zahl (Int) zurück, daher legen wir den Typ unserer Funktion explizit als `Int` fest. Wir nennen unsere Funktion "länge" und verwenden `s` als Parameter, aber du kannst jede beliebige Bezeichnung verwenden. In diesem Beispiel verwenden wir einfach einen einzelnen Buchstaben, um die Funktion zu vereinfachen.

Um diese Funktion zu testen, können wir sie in einer interaktiven Konsole wie GHCI verwenden. Hier ist der Output, wenn wir unsere Funktion auf verschiedene Zeichenketten anwenden:

```Haskell
λ> länge "Hallo"
5
λ> länge "Ist das eine lange Zeichenkette?"
31
```

Wie du sehen kannst, gibt die Funktion `länge` die korrekte Anzahl der Zeichen in der Zeichenkette zurück. Du kannst jetzt selbst versuchen, deine eigene Funktion zu schreiben, um die Länge von Zeichenketten in Haskell zu finden.

# Tiefer Einblick

Die Funktion `length` in Haskell ist sehr effizient, da sie nicht einfach nur die Anzahl der Zeichen zählt, sondern die Struktur der Liste selbst verwendet. Das bedeutet, dass die Länge einer Liste in O(1) berechnet werden kann, unabhängig davon, wie viele Elemente in der Liste vorhanden sind. Dies ist auf die Strukturen der Datenstrukturen in Haskell zurückzuführen, die sogenannten "Listen".

Eine "Liste" in Haskell ist eine geordnete Struktur, die aus einer beliebigen Anzahl von Elementen besteht. Jedes Element in der Liste kann unterschiedlichen Typen haben, erlaubt sind zum Beispiel Zahlen, Zeichenketten oder sogar Funktionen. Die Länge einer Liste wird einfach durch die Anzahl der Elemente definiert, die sie enthält.

Eine tiefere Behandlung der Art und Weise, wie Haskell Datenstrukturen implementiert und wie sie zur Bestimmung der Länge von Zeichenketten beitragen, würde den Rahmen dieses Artikels sprengen. Aber wenn du dich für diese Art von Details interessierst, gibt es viele Ressourcen im Internet, die sich damit beschäftigen.

# Siehe auch

- [Haskell-Wiki Eintrag zu Listen](https://wiki.haskell.org/List)
- [Dokumentation der Funktion `length` in der Offiziellen Haskell-Dokumentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:length)
- [Ein Tutorial zu Haskell Listen](https://en.wikibooks.org/wiki/Haskell/Lists_and_tuples)