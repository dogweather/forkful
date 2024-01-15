---
title:                "Eine Zeichenkette großschreiben"
html_title:           "Gleam: Eine Zeichenkette großschreiben"
simple_title:         "Eine Zeichenkette großschreiben"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte ich mir die Mühe machen, einen String zu verändern und zu großschreiben? Nun, es gibt viele Situationen, in denen es wichtig ist, dass Texte korrekt und einheitlich geschrieben sind. Zum Beispiel, wenn wir eine Benutzerschnittstelle erstellen oder Daten aus einer externen Quelle erhalten, die möglicherweise nicht richtig formatiert ist. Indem wir einen String in Großbuchstaben konvertieren, können wir sicherstellen, dass unser Code einheitlich und lesbar ist.

## Wie geht das?
Es gibt verschiedene Möglichkeiten, einen String in Gleam zu großzuschreiben. Die einfachste Methode ist die Verwendung der ```String.to_upper``` Funktion, die einen String in Großbuchstaben konvertiert. Zum Beispiel:

```gleam
let name = "sophie"
let name_in_caps = String.to_upper(name)
```

Dies würde ```"SOPHIE"``` als Ergebnis zurückgeben. Wenn wir einen längeren Text haben, können wir auch die ```String.map``` Funktion verwenden, um jeden Buchstaben im String individuell zu verändern. Hier ist ein Beispiel, bei dem wir ```map``` verwenden, um alle Buchstaben in einem String in Großbuchstaben umzuwandeln:

```gleam
let text = "Hallo, das ist ein Beispiel!"
let upper_text = String.map(to_upper, text)
```

Das Ergebnis wäre ```"HALLO, DAS IST EIN BEISPIEL!"```. Wir können auch die gleichen Funktionen verwenden, um einen String in Kleinbuchstaben umzuwandeln, indem wir anstelle von ```to_upper``` die ```to_lower``` Funktion benutzen.

## Tiefer gehend
Es ist wichtig zu beachten, dass die ```to_upper``` und ```to_lower``` Funktionen nicht nur für englische Buchstaben funktionieren. Sie funktionieren auch für jegliche Unicode-Zeichen, was bedeutet, dass sie für die meisten Sprachen und Schriftsysteme geeignet sind. Während es einfach erscheinen mag, einen String in Groß- oder Kleinbuchstaben umzuwandeln, ist es wichtig zu wissen, dass es auch zukünftige Entwicklungen geben kann, die diese Funktionen verbessern und erweitern werden.

## Siehe auch
- [Gleam Dokumentation zu Strings](https://gleam.run/book/tour/strings.html)
- [Tutorial zu String-Manipulation in Gleam](https://dev.to/dzcyb3r/updating-strings-in-gleam-8hj)
- [Offizielles Gleam Forum](https://github.com/gleam-lang/gleam/discussions)