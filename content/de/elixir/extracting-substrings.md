---
title:    "Elixir: Substrings extrahieren"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Substrings sind Teilzeichenfolgen, die aus einer größeren Zeichenfolge extrahiert werden. Im Elixir-Programmieren gibt es mehrere Gründe, warum man Substrings extrahieren würde. Zum Beispiel für die Textmanipulation oder die Verarbeitung von Benutzereingaben. In diesem Blogbeitrag werden wir uns ansehen, wie man Substrings in Elixir extrahiert.

## Wie geht man vor

Die Grundlage für die Extraktion von Substrings in Elixir ist die `String`-Funktion `slice/3`. Diese Funktion nimmt drei Argumente: die Zeichenfolge, aus der der Substring extrahiert werden soll, den Startindex und den Endindex. Der Startindex gibt an, an welcher Position in der Zeichenfolge der Substring beginnen soll, während der Endindex angibt, an welcher Position der Substring enden soll (ohne diesen Buchstaben einzuschließen). Hier ist ein Beispiel für die Verwendung von `slice/3`:

```Elixir
str1 = "Hallo, Welt!"
substring = String.slice(str1, 7, 10)
IO.puts(substring)
```

Dieser Code würde den Substring "Welt" extrahieren und ausgeben.

Wenn der Endindex nicht angegeben wird, werden alle Zeichen ab dem Startindex bis zum Ende der Zeichenfolge extrahiert. Hier ist ein Beispiel:

```Elixir
str2 = "Test123"
substring = String.slice(str2, 4)
IO.puts(substring)
```

Dieser Code würde die Zeichenfolge "123" extrahieren und ausgeben.

Man kann auch Negative Werte für Start- und Endindex verwenden, um den Substring aus dem Ende der Zeichenfolge zu extrahieren. Zum Beispiel:

```Elixir
str3 = "Hallo, Welt!"
substring = String.slice(str3, -5, -1)
IO.puts(substring)
```

Dieser Code würde den Substring "Welt" extrahieren und ausgeben.

## Tiefeneintauchen

Die Extraktion von Substrings in Elixir kann auch mithilfe regulärer Ausdrücke erfolgen. Hierfür wird die Funktion `Regex.run/3` verwendet, die eine Liste von Treffern zurückgibt. Hier ist ein Beispiel:

```Elixir
str4 = "1, 2, 3"
substring = Regex.run(~r/\d/, str4)
IO.inspect(substring)
```

Dieser Code würde eine Liste von Treffern zurückgeben, die aus den Zahlen in der Zeichenfolge besteht.

Eine interessante Funktion der `slice/3`-Funktion ist, dass man sie auch auf Listen anwenden kann. In diesem Fall werden die Elemente der Liste ab dem angegebenen Index ausgewählt. Zum Beispiel:

```Elixir
list = [1, 2, 3, 4]
substring = List.slice(list, 1)
IO.puts(substring)
```

Dieser Code würde eine Liste der Elemente ab dem Index 1 zurückgeben, also [2, 3, 4].

## Siehe auch

- [Elixir-Dokumentation zu `String.slice/3`](https://hexdocs.pm/elixir/String.html#slice/3)
- [Elixir-Dokumentation zu `Regex.run/3`](https://hexdocs.pm/elixir/Regex.html#run/3)
- [Elixir-Dokumentation zu `List.slice/2`](https://hexdocs.pm/elixir/List.html#slice/2)