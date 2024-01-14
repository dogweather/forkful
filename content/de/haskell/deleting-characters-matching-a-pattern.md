---
title:    "Haskell: Löschen von Zeichen mit entsprechendem Muster"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung gibt es oft die Notwendigkeit, bestimmte Zeichen in einem Text zu löschen, die einem bestimmten Muster entsprechen. Dies kann hilfreich sein, um unerwünschte Zeichen oder Leerzeichen zu entfernen oder um bestimmte Formate einzuhalten.

## Wie Funktioniert es

Die grundlegende Funktion, die wir verwenden werden, ist die `filter` Funktion, die in Haskell bereits vordefiniert ist. Diese Funktion ermöglicht es uns, eine Liste zu durchlaufen und Elemente basierend auf einer angegebenen Bedingung zu filtern.

Eine beispielhafte Verwendung dieser Funktion, um Zeichen aus einem String zu löschen, könnte folgendermaßen aussehen:

```haskell
deleteCharacters :: Char -> [Char] -> [Char]
deleteCharacters char = filter (/=char)
```

Wir können diese Funktion dann auf einen beliebigen String anwenden, zum Beispiel auf "Hallo Welt!" und bestimmen, welche Zeichen wir löschen möchten.

```haskell
deleteCharacters 'l' "Hallo Welt!"
```

Dies würde `"Hao Wett!"` ausgeben, da alle Vorkommen des Zeichens "l" aus dem String gelöscht werden.

## Tiefer Einblick

Um das Problem der Löschung von Zeichen noch weiter zu erläutern, möchten wir uns ein spezifischeres Beispiel anschauen. Angenommen, wir haben einen String mit Zahlen, die durch Leerzeichen getrennt sind, und möchten alle Leerzeichen löschen, um eine Datei ohne Leerzeichen zu erstellen.

```haskell
deleteCharacters ' ' "1 2 3 4 5"
```

Die Ausgabe dieses Codes würde dann `"12345"` ergeben, da alle Leerzeichen gelöscht wurden. Hierbei wird also die `filter` Funktion verwendet, um alle Leerzeichen aus der Liste zu entfernen.

## Siehe auch

- [Haskell Dokumentation über filter Funktion](https://www.haskell.org/tutorial/arrays.html#sect13.3)
- [Tutorial zu Zeichen und Zeichenreihen in Haskell](https://wiki.haskell.org/Character_and_string_explorations)