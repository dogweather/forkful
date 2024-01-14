---
title:    "Elm: Verketten von Zeichenfolgen"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Zusammenfügen oder Verketten von Strings ist eine wichtige Fähigkeit in vielen Programmiersprachen, einschließlich Elm. Es ermöglicht es Programmierern, dynamische Texte zu erstellen, die auf Variablen oder Benutzereingaben basieren. In diesem Blog-Beitrag werden wir uns genauer ansehen, wie man Strings in Elm konkateniert.

## Anleitung

Die Verkettung von Strings in Elm ist einfach und unkompliziert. Im Folgenden sind einige Beispiele aufgeführt, die die verschiedenen Möglichkeiten der String-Konkatenation zeigen.

```Elm
fullName : String
fullName = "Max" ++ " Mustermann"

-- Output: Max Mustermann
```

```Elm
message : String
message = "Hallo " ++ "Welt"

-- Output: Hallo Welt
```

```Elm
greeting : String -> String
greeting name =
    "Hello " ++ name ++ "!"

-- Output: Hello Max!
```

Wie Sie sehen können, können wir mit dem Operator "++" Strings einfach aneinanderreihen, um dynamische Nachrichten zu erstellen. Wir können auch Variablen oder Funktionen verwenden, um die Konkatenation noch flexibler zu gestalten.

## Tiefergehende Einblicke

In Elm gibt es auch die Funktion "String.concat", die es uns ermöglicht, Listen von Strings zu verkettet. Dies kann nützlich sein, wenn wir mehrere Strings in einem Stück zusammenführen möchten.

```Elm
names : List String
names = ["Max", "Sarah", "Tim"]

fullName : String
fullName = String.concat names

-- Output: MaxSarahTim
```

Es ist auch möglich, Strings in mehreren Schritten zu verkettet und dabei die Lesbarkeit zu verbessern. Zum Beispiel:

```Elm
firstName : String
firstName = "Max"

lastName : String
lastName = "Mustermann"

message : String
message =
    "Hello, my name is "
        ++ firstName
        ++ " "
        ++ lastName
        ++ "."

-- Output: Hello, my name is Max Mustermann.
```

## Siehe auch

- [Offizielle Elm-Dokumentation zu Strings](https://guide.elm-lang.org/strings/)
- [Einführung in Elm – eine funktionale Programmiersprache für das Frontend](https://www.codementor.io/@sarahlieder/intro-to-elm-a-functional-programming-language-for-the-frontend-q517gakn0)