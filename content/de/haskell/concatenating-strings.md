---
title:                "Strings verketten"
html_title:           "Haskell: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Wenn du jemals eine einfache Möglichkeit gesucht hast, um Strings in Haskell zu verbinden, dann bist du hier richtig. Das Verketten von Strings ist eine häufige Aufgabe beim Programmieren und kann in verschiedenen Situationen nützlich sein, wie z.B. beim Zusammenbauen von Benutzeranweisungen oder beim Erstellen von Ausgabe für den Endbenutzer.

## Wie geht man vor

Um Strings in Haskell zu verbinden, gibt es verschiedene Möglichkeiten. Eine Möglichkeit ist die Verwendung der Funktion `++`, die zwei Strings zusammenfügt. Hier ist ein Beispielcode:

```Haskell
concatenate :: String -> String -> String
concatenate str1 str2 = str1 ++ str2

main = do
  print (concatenate "Hallo" "Welt")
```

Das obere Beispiel definiert eine Funktion `concatenate`, die zwei Strings als Parameter annimmt und sie mit dem Operator `++` verbindet. In der `main`-Funktion wird diese Funktion aufgerufen und das Ergebnis `"HalloWelt"` ausgegeben.

Eine andere Möglichkeit ist die Verwendung der Funktion `concat`, die eine Liste von Strings entgegennimmt und sie alle zusammenfügt. Hier ist ein Beispielcode:

```Haskell
concatenateList :: [String] -> String
concatenateList list = concat list

main = do
  print (concatenateList ["H", "a", "l", "l", "o"])
```

Das obere Beispiel definiert eine Funktion `concatenateList`, die eine Liste von Strings annimmt und sie mit der Funktion `concat` zusammenfügt. In der `main`-Funktion wird diese Funktion aufgerufen und das Ergebnis `"Hallo"` ausgegeben.

## Tiefergehender Einblick

Im obigen Beispiel wurde `++` verwendet, um Strings zu verbinden. Allerdings ist zu beachten, dass der Operator `++` im Hintergrund eine Funktion aufruft, um die Strings zu verbinden. Diese Funktion ist `append`, die in der Standardbibliothek von Haskell enthalten ist.

Außerdem ist es wichtig zu beachten, dass Strings in Haskell unveränderbar sind, was bedeutet, dass jedes Mal, wenn eine Verkettung durchgeführt wird, ein neues String-Objekt erzeugt wird. Dies kann zu Problemen mit der Leistung führen, wenn es in Schleifen oder in einer großen Anzahl von Verkettungen verwendet wird. Um dies zu vermeiden, sollten effizientere Datenstrukturen wie `Text` oder `ByteString` verwendet werden.

## Siehe auch

- [Offizielle Dokumentation zu Strings in Haskell](https://www.haskell.org/tutorial/string.html)
- [GHC-Bibliothek für Textverarbeitung](https://hackage.haskell.org/package/text)
- [Effiziente Arbeitsweise mit großen Strings in Haskell](https://wiki.haskell.org/WikiRules/String)