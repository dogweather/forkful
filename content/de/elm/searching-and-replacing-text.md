---
title:                "Elm: Suchen und Ersetzen von Texten"
simple_title:         "Suchen und Ersetzen von Texten"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum 

Es gibt viele Gründe, warum man sich möglicherweise mit der Suche und Ersetzung von Text beschäftigen möchte. Egal ob man herkömmliche Textdateien bearbeitet oder in der Softwareentwicklung tätig ist, die Möglichkeit, Text in großem Umfang zu ändern, kann sehr nützlich sein. Mit Elm können wir diese Aufgabe schnell und effizient bewältigen.

## Wie geht man vor?

Um Text in Elm zu suchen und zu ersetzen, können wir die `String.replace` Funktion verwenden. Diese akzeptiert drei Argumente: eine Zeichenkette, nach der gesucht werden soll, eine Zeichenkette, die die Suche ersetzen soll, und den ursprünglichen Text. Zum Beispiel:

```Elm
String.replace "Hallo" "Guten Tag" "Hallo Welt!" 

-- Ausgabe: "Guten Tag Welt!"
```

Wenn keine Übereinstimmung gefunden wird, wird der ursprüngliche Text einfach zurückgegeben. Man kann jedoch auch spezifizieren, wie viele Übereinstimmungen ersetzt werden sollen, indem man ein fünftes Argument hinzufügt, das die maximale Anzahl der Ersetzungen festlegt.

```Elm
String.replace "a" "o" "Banane" 1 

-- Ausgabe: "Bonane"
```

Es ist auch möglich, reguläre Ausdrücke für die Suche und Ersetzung von Text in Elm zu verwenden. Dafür können wir die `Regex.replace` Funktion nutzen. Diese verhält sich ähnlich wie `String.replace`, erlaubt uns aber die Verwendung von regulären Ausdrücken, um unsere Suche zu verfeinern.

```Elm
Regex.replace (Regex.regex "\\s+") " " "Elm ist        eine wunderbare Programmiersprache" 

-- Ausgabe: "Elm ist eine wunderbare Programmiersprache"
```

## Ein tieferer Einblick

Bei der Suche und Ersetzung von Text in Elm gibt es noch einige weitere Funktionen und Optionen zu beachten. Zum Beispiel können wir mit `String.split` Text basierend auf bestimmten Trennzeichen oder Mustern in eine Liste von Zeichenketten aufteilen. Umgekehrt können wir mit `String.join` eine Liste von Zeichenketten in einen Text zusammenführen. Und mit `String.contains` können wir überprüfen, ob eine Zeichenkette in einer anderen enthalten ist.

Ein weiteres nützliches Feature ist die Möglichkeit, Funktionen in Elm zu verwenden, um die Werte zu ersetzen, anstatt nur statische Zeichenketten zu verwenden. Auf diese Weise können wir Text basierend auf dynamischen Variablen oder berechneten Werten ersetzen, was unsere Anwendungen noch flexibler macht.

## Siehe auch

- Offizielle Elm Dokumentation zu `String`
- Einführung zu regulären Ausdrücken in Elm 
- Elm-Forum: Austausch über die Verwendung von `String.replace`