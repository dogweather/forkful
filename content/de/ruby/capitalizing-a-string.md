---
title:    "Ruby: Einen String in Großbuchstaben umwandeln."
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Warum

Jeder, der schon einmal mit Strings in Ruby gearbeitet hat, weiß wie wichtig es ist, sie richtig zu formatieren. Eine gängige Anforderung dabei ist es, den ersten Buchstaben jedes Wortes in einem String groß zu schreiben. Dies kann aus ästhetischen oder aus Gründen der Lesbarkeit notwendig sein. In diesem Artikel werden wir uns genauer ansehen, wie man einen String in Ruby kapitalisiert.

## Wie funktioniert es?

Die einfachste Möglichkeit, einen String in Ruby zu kapitalisieren, ist die Verwendung der `capitalize` Methode. Diese Methode macht den ersten Buchstaben groß und den Rest des Strings klein.

```Ruby
name = "max mustermann"
puts name.capitalize
```
Output: Max mustermann

Wir können auch `capitalize!` verwenden, um den String direkt zu verändern:

```Ruby
name = "max mustermann"
name.capitalize!
puts name
```
Output: Max mustermann

Wenn wir nur den ersten Buchstaben eines Satzes oder einer Phrase groß schreiben möchten, können wir die `upcase` Methode verwenden:

```Ruby
motto = "lebe wild und gefährlich"
puts motto.upcase
```
Output: LEBE WILD UND GEFÄHRLICH

## Tief Luftholen

Es gibt verschiedene Möglichkeiten, einen String in Ruby zu kapitalisieren, je nachdem, wie komplex die Formatierung sein soll. Wenn wir unsere Strings auf bestimmte Weise formatieren wollen, können wir die `gsub` Methode verwenden und eine Regular Expression übergeben.

Zum Beispiel, wenn wir den String "123 main street" so formatieren möchten, dass jeder erste Buchstabe in einem Wort großgeschrieben wird, können wir dies tun:

```Ruby
address = "123 main street"
puts address.gsub(/\b\w/, &:upcase)
```
Output: 123 Main Street

Wir können auch `split` verwenden, um den String in einzelne Wörter zu zerlegen und dann jeden ersten Buchstaben groß zu schreiben:

```Ruby
address = "123 main street"
formatted_address = address.split(" ").map(&:capitalize).join(" ")
puts formatted_address
```
Output: 123 Main Street

## Siehe auch

- [Ruby String Documentation](https://ruby-doc.org/core-2.6/String.html)
- [Ruby Regular Expressions](https://ruby-doc.org/core-2.6/Regexp.html)
- [String transformation methods in Ruby](https://www.geeksforgeeks.org/string-transformation-methods-in-ruby/)