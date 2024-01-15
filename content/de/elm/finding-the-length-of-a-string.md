---
title:                "Die Länge eines Strings finden"
html_title:           "Elm: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum
Die Länge einer Zeichenkette zu finden kann in vielen Programmierprojekten von entscheidender Bedeutung sein, zum Beispiel beim Überprüfen von Benutzereingaben oder bei der Manipulation von Textdaten.

# Wie das geht

```Elm
value : String
value = "Dies ist eine Beispielzeichenkette"

length : Int
length = String.length value

main =
  text (toString length) -- Output: 30
```

Wie in unserem Beispiel zu sehen ist, können wir mit der `String.length` Funktion die Länge einer Zeichenkette in Elm finden. Diese Funktion gibt uns ein Integer zurück, der die Anzahl der in der Zeichenkette enthaltenen Zeichen repräsentiert. Wir können dann diesen Wert mit `toString` in einen String umwandeln, um ihn mit `text` auszugeben.

# Tiefer Einblick
Bevor wir die `String.length` Funktion nutzen können, müssen wir verstehen, wie Zeichenketten in Elm dargestellt werden. Elm verwendet Unicode zur Codierung von Zeichenketten, was bedeutet, dass jedes Zeichen in einer Zeichenkette einen numerischen Code hat, der es repräsentiert. Ein einzelnes Zeichen kann jedoch aus mehreren Codepunkten bestehen, was seine tatsächliche Darstellung beeinflusst. Dies ist wichtig zu wissen, da die `String.length` Funktion die Anzahl der Codepunkte in einer Zeichenkette zurückgibt, nicht die Anzahl der sichtbaren Zeichen.

Ein weiteres wichtiges Konzept beim Finden der Länge einer Zeichenkette ist die Verwendung von `toString`. Diese Funktion konvertiert jedes gegebene Argument in einen String, unabhängig von seinem Datentyp. Daher müssen wir `toString` verwenden, um den Integer-Wert, den wir von `String.length` erhalten, in einen String umzuwandeln, damit wir ihn mit `text` ausgeben können.

# Siehe auch

- [Elm Dokumentation zu Strings](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Offizielles Elm Guide](https://guide.elm-lang.org/)
- [Codebeispiele auf Ellie](https://ellie-app.com/new) (Online-Elm-Compiler zum Ausprobieren)