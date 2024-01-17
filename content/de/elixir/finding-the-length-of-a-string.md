---
title:                "Die Länge eines Strings finden"
html_title:           "Elixir: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Finden der Länge einer Zeichenkette ist ein nützliches Tool in der Programmierung, da es uns ermöglicht, die Anzahl der Zeichen in einer Zeichenkette zu bestimmen. Dies ist besonders hilfreich, wenn wir Texte oder Eingaben verarbeiten müssen.

## Wie geht es?
```Elixir
string = "Hallo Welt"
IO.puts("Die Länge von '#{string}' ist: #{String.length(string)}")
```
Output: 
```
Die Länge von 'Hallo Welt' ist: 10
```

```Elixir
empty_string = ""
IO.puts("Die Länge von '#{empty_string}' ist: #{String.length(empty_string)}")
```
Output:
```
Die Länge von '' ist: 0
```
Wie man sehen kann, können wir einfach die Funktion `String.length` verwenden, um die Länge einer Zeichenkette zu bestimmen.

## Tiefgang
Das Finden der Länge einer Zeichenkette ist eine grundlegende Funktion in vielen Programmiersprachen. In Elixir wird die Länge einer Zeichenkette durch die Anzahl der Unicode-Zeichen in der Zeichenkette bestimmt. Es ist wichtig zu beachten, dass die Länge einer Zeichenkette und die Anzahl der Bytes in der Zeichenkette nicht unbedingt übereinstimmen müssen, da manche Zeichen mehrere Bytes benötigen.

Alternativ können wir auch die Funktion `Enum.count` verwenden, um die Anzahl der Elemente in einer Liste zu bestimmen, die in diesem Fall die Zeichen in einer Zeichenkette wären. Allerdings ist es effizienter, direkt `String.length` zu verwenden, da dies eine spezielle Funktion für Zeichenketten ist.

## Siehe auch
Weitere Informationen zu Zeichenketten in Elixir und verwandten Funktionen finden Sie in der offiziellen Elixir-Dokumentation: 
- [Elixir Zeichenketten-Dokumentation](https://hexdocs.pm/elixir/String.html)
- [Elixir Enum-Dokumentation](https://hexdocs.pm/elixir/Enum.html#content)