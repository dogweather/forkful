---
title:                "Die Länge einer Zeichenkette finden"
html_title:           "Gleam: Die Länge einer Zeichenkette finden"
simple_title:         "Die Länge einer Zeichenkette finden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge eines Strings ist die Anzahl der Zeichen, aus denen er besteht. Programmierer müssen oft die Länge eines Strings herausfinden, um beispielsweise die korrekte Speicherzuweisung oder die richtige Verarbeitung von Eingaben zu gewährleisten.

## Wie geht's?
Um die Länge eines Strings in Gleam zu finden, können wir die eingebaute Funktion `String.len` verwenden. Hier ist ein Beispielcode, der die Länge eines Strings mithilfe dieser Funktion ausgibt:

```Gleam
let string = "Hallo, Welt!"
let length = String.len(string)
```

Die Variable `length` enthält nun die Zahl 13, da das Wort "Hallo, Welt!" 13 Zeichen lang ist.

## Tiefer Einblick
Die Idee, die Länge eines Strings zu finden, stammt aus einer Zeit, in der Computer noch nicht so leistungsfähig waren wie heute. Damals mussten Programmierer sorgfältig Ressourcen verwalten, um die effizientesten Programme zu schreiben. Heutzutage ist die Nutzung integrierter Funktionen wie `String.len` die bevorzugte Methode, um die Länge eines Strings zu finden. Eine alternative Möglichkeit ist die Verwendung von Schleifen mit der Zählung der Zeichen, die jedoch weniger effizient ist.

## Siehe auch
Weitere Informationen zu Strings und deren Verarbeitung finden Sie in der offiziellen Gleam-Dokumentation unter [String functions](https://gleam.run/documentation/stdlib/String.html#functions).