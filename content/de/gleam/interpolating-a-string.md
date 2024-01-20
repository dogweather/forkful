---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Interpolation von Zeichenketten ermöglicht es Programmierern, Variablen oder Ausdrücke in Zeichenketten einzufügen, um dynamischen Text zu erzeugen. Dies ist hilfreich, um lesbaren und wartbaren Code zu schreiben und um die Notwendigkeit manueller Zeichenkettenoperationen zu beseitigen.

## So geht's:

Hier ist ein einfaches Beispiel für die Interpolation von Zeichenketten in Gleam:

```Gleam
fn main() {
  let name = "Gleam"
  let message = "Willkommen bei {name}!"
  io.println(message) 
}
```
Dieser Code gibt "Willkommen bei Gleam!" aus.

## Hintergrundinformationen

1. Historischer Kontext: Interpolation von Zeichenketten ist kein neues Konzept. Es wurde in vielen Programmiersprachen wie Perl, Ruby, Python und nun auch in Gleam verwendet.

2. Alternativen: Man könnte die `++` Funktion verwenden, um Zeichenketten in Gleam zu verketten, die aber weniger lesbar ist und mehr Schreibarbeit erfordert.

3. Implementierungsdetails: Gleam verwendet die `{}` Platzhalter Syntax für die Interpolation von Zeichenketten und ersetzt diese Platzhalter dynamisch zur Laufzeit.

## Siehe auch

- Gleam Dokumentation: [Gleam String Interpolation](https://gleam.run/book/tour/strings.html)
- Blogpost: [Introduction to Gleam](https://gleam.run/news/gleam-v0.8-released/)