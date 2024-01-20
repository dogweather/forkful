---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Ermittlung der Länge eines Strings ist ein Prozess, bei dem gezählt wird, wie viele Zeichen in einem bestimmten Textabschnitt vorhanden sind. Programmierer machen das oft, um eine genaue Kontrolle über ihre Daten zu haben und Redundanzen zu vermeiden, die Speicher verbrauchen könnten.

## Wie man es macht:

Hier ist ein einfaches Gleam-Beispiel, um die Länge eines Strings zu finden:

```Gleam
import gleam/string

let my_string = "Hallo Welt"
let string_length = my_string |> string.len
string_length |> Int.to_string |> Io.format("The string length is {}\n")
```
Wenn du diesen Code ausführst, siehst du die Ausgabe: 
```
The string length is 11
```

## Tiefer eintauchen:

1. Historischer Kontext: Die Ermittlung der String-Länge ist eine grundlegende Funktion, die in fast jeder Programmiersprache vorhanden ist. In Gleam ist sie als Funktion in der String-Bibliothek eingebaut.

2. Alternativen: In einigen Fällen könntest du eine manuelle Schleifenkonstruktion verwenden, um die Länge eines Strings zu finden, aber die eingebaute Funktion ist meist effizienter und leichter zu lesen.

3. Implementierungsdetails: Die Gleam-Implementierung der String-Länge basiert auf der zugrunde liegenden Erlang-Implementierung, die die Länge in konstanter Zeit O(1) findet. 

## Siehe auch:

Einige nützliche Links für weitere Informationen:
- Gleam Language-Github: https://github.com/gleam-lang/gleam
- Gleam Standard Library: https://hexdocs.pm/gleam_stdlib/readme.html
- Erlang String-Module: http://erlang.org/doc/man/string.html
- Ein Einführungskurs in Gleam: https://gleam.run/tour/