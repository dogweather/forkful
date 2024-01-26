---
title:                "Ermittlung der Zeichenkettenlänge"
date:                  2024-01-20T17:47:34.531203-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ermittlung der Zeichenkettenlänge"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge eines Strings zu finden bedeutet, die Anzahl der Zeichen darin zu ermitteln. Programmierer brauchen diese Information, um Validierung, Formatierung oder andere Operationen durchzuführen, bei denen die genaue Stringlänge wichtig ist.

## Wie geht das:
```gleam
import gleam/string

fn main() {
  let greeting = "Hallo Welt!"
  let length = string.len(greeting)
  io.println(length) // Gibt 11 aus, denn "Hallo Welt!" hat 11 Zeichen
}
```

## Tiefgang
In älteren oder niedrigeren Programmiersprachen, wie C, ist das Finden der Länge eines Strings komplizierter, da man selbst durch den Speicher navigieren muss, bis man auf das Null-Zeichen trifft. In Gleam und anderen modernen Sprachen übernimmt diese Funktion die Standardbibliothek, meist mit konstanter Laufzeit (O(1)), da die Länge intern gehalten wird. Alternative Methoden, wie das Iterieren über einen String, um die Zeichen manuell zu zählen, sind in Gleam unnötig und weniger effizient.

## Siehe auch
- The official Gleam language guide: [https://gleam.run/book](https://gleam.run/book)
