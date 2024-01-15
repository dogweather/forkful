---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "Haskell: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit dem Löschen von Zeichen, die einer bestimmten Musterung entsprechen, beschäftigen? Einfach ausgedrückt: Es ist eine nützliche Fähigkeit für die Manipulation von Texten in Haskell.

## Wie geht man vor?

Um Zeichen basierend auf einem Muster zu löschen, können wir die `delete` Funktion aus dem `Data.List` Modul verwenden. Als Beispiel nehmen wir einen String und wollen alle Leerzeichen aus diesem String entfernen. Hier ist der Code:

```Haskell
import Data.List

stringWithoutSpaces = delete ' ' "Dies ist ein Beispieltext."
-- "DiesisteinBeispieltext."
```

Wie du sehen kannst, haben wir einfach das Leerzeichen als erstes Argument in die `delete` Funktion eingegeben und den String als zweites Argument. Das Ergebnis ist ein String ohne Leerzeichen.

Natürlich funktioniert dieses Prinzip nicht nur für Leerzeichen, sondern für alle Zeichen, die du aus einem String entfernen möchtest. Du kannst sogar ganze Wörter oder Zahlen löschen, wenn du sie als Charakter konvertierst.

```Haskell
import Data.List

deleteNumbers = delete '1' "12345"
-- "2345"

deleteWord = delete 'e' "Haskell"
-- "Haskll"
```

## Tiefergehende Informationen

Die `delete` Funktion verwendet das `Eq` Typenklasse, um zu überprüfen, ob ein Zeichen im String vorkommt. Das bedeutet, dass wir die `delete` Funktion auf jedem Datentyp anwenden können, der eine Instanz von `Eq` hat. Das schließt auch eigene Datentypen ein, solange wir `Eq` für diese Typen definieren.

```Haskell
data Person = Person { name :: String, age :: Int } deriving Eq

people = [Person "Max" 25, Person "Lisa" 30, Person "John" 40]

deletePerson = delete (Person "Lisa" 30) people
-- [Person "Max" 25, Person "John" 40]
```

Eine weitere nützliche Funktion für das Löschen von Zeichen basierend auf einem Muster ist `filter`. Diese Funktion nimmt ein Prädikat (eine Funktion, die ein Bool zurückgibt) und eine Liste als Argumente und gibt eine neue Liste zurück, die nur die Elemente enthält, die das Prädikat erfüllen.

```Haskell
filterNumbers = filter (\x -> x `mod` 2 == 0) [1,2,3,4,5] -- x `mod` 2 == 0 ist das Prädikat für gerade Zahlen
-- [2,4]
```

Möchtest du alle Vorkommen eines Zeichens in einem String löschen, kannst du `deleteAll` aus dem `Data.List.Extra` Modul verwenden.

```Haskell
import Data.List.Extra
deleteAllLetters = deleteAll 'a' "abcdabca"
-- "bdc"
```

## Siehe auch

- [Haskell Wiki - Strings](https://wiki.haskell.org/Handling_strings)
- [Haskell Programming from First Principles - String Operations](http://haskellbook.com/chapters/strings.html)
- [Haskell.org - Data.List Module](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)