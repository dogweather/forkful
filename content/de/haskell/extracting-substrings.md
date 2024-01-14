---
title:    "Haskell: Unterstrings extrahieren"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings ist eine häufige Aufgabe beim Programmieren mit Haskell. Durch das Extrahieren von Teilen eines Strings, können wir Texte effizient durchsuchen, ersetzen oder transformieren. Dies ist besonders hilfreich bei der Verarbeitung von großen Datensätzen oder beim Schreiben von Parsern für komplexe Dateiformate. Aber wie können wir dies in Haskell umsetzen?

## Wie geht man vor?

Die Basisfunktion für das Extrahieren von Teilstrings in Haskell ist `take`. Diese Funktion nimmt einen numerischen Wert als Argument und gibt die ersten n Zeichen eines Strings zurück. Zum Beispiel:

```Haskell
take 3 "Hallo, Welt!" 
```

Die Ausgabe wäre:

```Haskell
"Hal"
```

Wir können auch den `drop` Befehl nutzen, welcher das Gegenteil von `take` ist. Er gibt die verbleibenden Zeichen des Strings nach der gegebenen Anzahl zurück. Zum Beispiel:

```Haskell
drop 6 "Hallo, Welt!"
```

Die Ausgabe wäre:

```Haskell
"Welt!"
```

Diese beiden Funktionen sind nützlich, um einen bestimmten Bereich eines Strings zu extrahieren. Aber was ist, wenn wir nach einem bestimmten Muster suchen und dann den entsprechenden Teilstring extrahieren wollen?

Eine Möglichkeit dies zu tun ist die Verwendung der `break` Funktion. Diese Funktion nimmt ein prädikatives Argument, das auf jedes Zeichen des Strings angewendet wird. Wenn das prädikative Argument `True` zurückgibt, wird der String bis zu dieser Stelle extrahiert. Zum Beispiel:

```Haskell
break (=='!') "Hallo, Welt!"
```

Die Ausgabe wäre:

```Haskell
("Hallo, Welt", "!")
```

Dies ist besonders nützlich, wenn wir nach einem bestimmten Trennzeichen in einem String suchen und den Teil davor extrahieren wollen.

## Tieferes Eintauchen

Jetzt, da wir die Grundlagen des Extrahierens von Teilstrings kennen, können wir uns mit komplexeren Anwendungen befassen. Eine mögliche Anwendung ist das Parsen von Datumsinformationen aus einem String. Hier können wir die `span` Funktion nutzen, um ein Datum von einem String zu extrahieren. Diese Funktion nimmt ein prädikatives Argument, aber im Gegensatz zu `break` gibt sie den String zurück, bis das prädikative Argument `False` zurückgibt. Zum Beispiel:

```Haskell
span isDigit "12.05.2020"
```

Die Ausgabe wäre:

```Haskell
("12", ".05.2020")
```

Wir können auch die `words` Funktion verwenden, um einen String in einzelne Wörter aufzuteilen und dann entsprechend zu manipulieren. Zum Beispiel:

```Haskell
words "Hallo, Welt!"
```

Die Ausgabe wäre:

```Haskell
["Hallo,", "Welt!"]
```

Und schließlich können wir die `unwords` Funktion verwenden, um eine Liste von Strings wieder zu einem einzelnen String zusammenzufügen. Zum Beispiel:

```Haskell
unwords ["Hallo,", "Welt!"]
```

Die Ausgabe wäre:

```Haskell
"Hallo, Welt!"
```

## Sieh auch

Hier sind einige nützliche Ressourcen, um tiefer in das Thema des Extrahierens von Teilstrings in Haskell einzutauchen:

- [Offizielles Haskell-Tutorial](https://www.haskell.org/tutorial/strings.html)
- [Hoogle - Haskell-Dokumentation und Suchmaschine](https://hoogle.haskell.org/)
- [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell/String_Manipulation)