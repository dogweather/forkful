---
title:    "Elm: String in Kleinschreibung umwandeln"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum?

String zu Kleinbuchstaben konvertieren? Das ist eine einfache Aufgabe, aber sie kann nützlich sein, wenn du zum Beispiel Nutzereingaben in einem Formular verarbeiten möchtest oder wenn du Daten aus einer API abrufst und in einem einheitlichen Format darstellen möchtest.

## Wie?

```Elm
-- Eine Funktion zum Konvertieren von String zu Kleinbuchstaben
toLowercase : String -> String
toLowercase str =
    String.toLower str

-- Beispielausgabe
toLowercase "ELM PROGRAMMIERUNG"
-- "elm programmierung"
```

## Tiefer gehende Informationen

In Elm gibt es bereits eine eingebaute Funktion `String.toLower`, die einen String in Kleinbuchstaben umwandelt. Sie verwendet den Unicode-Standard, um auch nicht-lateinische Buchstaben richtig zu konvertieren.

Zusätzlich zu dieser Funktion bietet die Elm-Standardbibliothek auch `String.toLowercase`, die speziell auf die Verarbeitung von deutschen Umlauten und Sonderzeichen optimiert ist.

Wenn du dich tiefer mit der String-Konvertierung in Elm beschäftigen möchtest, empfehle ich dir die offizielle Dokumentation und die Quellcode-Dateien der Standardbibliothek zu durchsuchen.

## Siehe auch
- [Offizielle Elm-Dokumentation zum Thema "Strings"](https://elm-lang.org/docs/strings)
- [Quellcode der Elm-Standardbibliothek für Strings](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Weiterführender Blogartikel "Handling Strings in Elm"](https://dennisreimann.de/articles/handling-strings-in-elm.html)