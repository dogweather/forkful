---
title:                "Elm: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Warum
Jeder, der schon einmal Programmcode geschrieben hat, weiß wie wichtig die korrekte Schreibweise von Variablen und Funktionen ist. Manchmal jedoch kann es vorkommen, dass man einen String in Großbuchstaben hat, aber ihn in Kleinbuchstaben umwandeln muss. Das ist genau dann der Fall, wenn man beispielsweise Benutzereingaben vergleichen möchte oder eine einheitliche Formatierung benötigt.

# Anleitung
Um einen String in Elm in Kleinbuchstaben umzuwandeln, gibt es eine eingebaute Funktion namens `String.toLower`. Sie nimmt einen String entgegen und gibt einen neuen String in Kleinbuchstaben zurück. Hier ist ein Beispielcode:
```Elm
myString = "ELM PROGRAMMIEREN IST TOLL!"
lowercaseString = String.toLower myString
```

Die Variable `lowercaseString` enthält nun den Wert "elm programmieren ist toll!".

# Tiefgründiger Einblick
Intern verwendet die `String.toLower` Funktion das Unicode-Zeichensatzstandard, um zu bestimmen, wie ein Buchstabe in Kleinbuchstaben konvertiert wird. Dies bedeutet, dass auch Sonderzeichen und Buchstaben mit Akzenten korrekt umgewandelt werden. Deshalb ist es wichtig, diese Funktion zu verwenden, anstatt eigene Methoden für die Konvertierung zu schreiben.

# Siehe auch
- [Elm Dokumentation über String.toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Blogbeitrag: Strings in Elm](https://thoughtbot.com/blog/strings-in-elm)