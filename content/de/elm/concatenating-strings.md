---
title:                "Elm: Verketten von Zeichenketten"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung gibt es oft die Notwendigkeit, Texte zu verbinden, um einen längeren String zu erzeugen. Dies kann hilfreich sein, um Benutzereingaben zu speichern, komplexe Fehlermeldungen zu generieren oder einfach nur Text auf einer Benutzeroberfläche darzustellen. In Elm gibt es verschiedene Möglichkeiten, Strings zu verbinden, je nach den spezifischen Anforderungen des Programms. In diesem Artikel werden wir uns genauer ansehen, wie man Strings in Elm concatenieren kann.

## Wie geht das?

Um Strings in Elm zu concatenieren, gibt es zwei Hauptmethoden: die Verwendung des (+++) Operators und die Verwendung der String.concat Funktion. Der (+++) Operator fügt zwei Strings zusammen, während die String.concat Funktion eine Liste von Strings in einen einzelnen String zusammenfügt.

### (+++) Operator Beispiel:

```Elm
"Hello " +++ "World"
```

Ausgabe: "Hello World"

### String.concat Beispiel:

```Elm
String.concat ["Hello ", "World"]
```

Ausgabe: "Hello World"

In beiden Fällen können auch Variablen oder Funktionen verwendet werden, die Strings zurückgeben, anstatt die Strings direkt einzugeben.

Die Verwendung des (+++) Operators ist in der Regel hilfreicher, wenn nur wenige Strings concateniert werden müssen. Wenn jedoch eine größere Anzahl von Strings zusammengefügt werden muss, ist die Verwendung der String.concat Funktion effizienter, da sie eine Liste von Strings bearbeiten kann.

## Tiefer eintauchen

In Elm gibt es auch die Möglichkeit, Strings mit Mustern zu concatenieren. Dazu werden Muster angegeben, die angeben, welche Teile der zu concatenierenden Strings hinzugefügt oder entfernt werden sollen. Dies kann besonders nützlich sein, wenn komplexe Textmanipulation erforderlich ist, z.B. beim Zusammenfügen von Teilen einer URL.

Ein Beispiel für die Verwendung von Mustern beim Concatenieren von Strings:

```Elm
mustache = "mustache"
beard = "beard" 

concatWithPattern = 
    case (mustache, beard) of
        ("mustache", "beard") ->
            mustache ++ " & " ++ beard
        (_, _) ->
            "No facial hair found."
```

Ausgabe: "mustache & beard"

## Siehe auch

- "Official Elm Language Guide" (https://guide.elm-lang.org/)
- "Elm in Action" von Richard Feldman (https://www.manning.com/books/elm-in-action)
- "Elm Tutorial" von Derek Cicerone (https://www.elm-tutorial.org/de/)