---
title:    "Elixir: Die Länge eines Strings finden"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge einer Zeichenkette scheint auf den ersten Blick eine einfache Aufgabe zu sein, aber es ist ein grundlegender Schritt beim Schreiben von effektivem Code. Es ist wichtig, die Länge einer Zeichenkette zu kennen, um z.B. die richtige Formatierung oder Validierung von Eingaben zu gewährleisten.

## Wie man die Länge einer Zeichenkette in Elixir finden kann

```Elixir
# Beispieldaten definieren
string = "Hallo Welt!"

# Verwendung der Funktion String.length()
length = String.length(string)

# Ausgabe der Länge der Zeichenkette
IO.puts("Die Länge der Zeichenkette ist: #{length}")

# Ausgabe: Die Länge der Zeichenkette ist: 11
```

Um die Länge einer Zeichenkette in Elixir zu finden, können wir die vordefinierte Funktion `String.length()` verwenden. Diese Funktion gibt die Anzahl der Zeichen in einer Zeichenkette zurück.

## Tiefere Einblicke

Beim Finden der Länge einer Zeichenkette ist es wichtig zu beachten, dass nicht alle Zeichen gleich gezählt werden. In Elixir werden Zeichen mit einzelnen Bytes gezählt, was bedeutet, dass z.B. Buchstaben mit Akzenten oder spezielle Zeichen möglicherweise aus mehr als einem Byte bestehen und somit als mehrere Zeichen gezählt werden.

Eine weitere wichtige Funktion beim Umgang mit Zeichenketten ist `String.codepoints()`, die eine Liste der einzelnen Codepoints (numerische Darstellung eines Zeichens) zurückgibt. Mit dieser Funktion können wir die Anzahl der Zeichen in einer Zeichenkette genauer ermitteln.

## Siehe auch

- [Elixir Dokumentation zu String.length()](https://hexdocs.pm/elixir/String.html#length/1)
- [Elixir Dokumentation zu String.codepoints()](https://hexdocs.pm/elixir/String.html#codepoints/1)