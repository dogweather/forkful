---
title:    "Elixir: Umwandeln einer Zeichenkette in Kleinbuchstaben"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals mit Strings in Ihrer Elixir-Codebasis gearbeitet haben, haben Sie sich vielleicht gefragt, ob es möglich ist, einen String in Kleinbuchstaben zu konvertieren. In diesem Blogbeitrag werden wir genau das diskutieren - warum es sinnvoll ist, Strings in Kleinbuchstaben zu konvertieren und wie Sie dies in Elixir erreichen können.

## Wie es geht

Um einen String in Kleinbuchstaben zu konvertieren, können Sie die Funktion `String.downcase/1` verwenden. Diese Funktion akzeptiert einen String als Argument und gibt eine neue Version des Strings zurück, in der alle Buchstaben in Kleinbuchstaben geschrieben sind.

```Elixir
iex> String.downcase("ELIXIR IST TOLL")
"elixir ist toll"
iex> String.downcase("1234")
"1234"
```

Wie Sie sehen können, funktioniert die Funktion auch mit Zahlen und gibt sie unverändert zurück. Es ist wichtig zu beachten, dass diese Funktion nur ASCII-Zeichen in Kleinbuchstaben umwandelt. Wenn Sie Sonderzeichen oder Umlaute haben, müssen Sie zuerst die Funktion `String.normalize/1` verwenden, um sie in ASCII umzuwandeln, bevor Sie `String.downcase/1` anwenden.

## Tiefergehende Informationen

In Elixir verwenden Strings UTF-8-Kodierung, was bedeutet, dass sie Unicode-Zeichen unterstützen. Dies grenzt Elixir von anderen Programmiersprachen ab, die normalerweise ASCII oder UTF-16 verwenden. Wenn Sie also einen String in Kleinbuchstaben konvertieren, kann dies zu unerwarteten Ergebnissen führen, wenn Sie mit Nicht-ASCII-Zeichen arbeiten.

Um dies zu vermeiden, sollten Sie `String.lowercase/1` verwenden, die speziell für Unicode-Zeichen ausgelegt ist und alle Zeichen ordnungsgemäß in Kleinbuchstaben konvertiert.

```Elixir
iex> String.lowercase("ÄPFEL")
"äpfel"
```

## Siehe auch

- Dokumentation zu [`String.downcase/1`](https://hexdocs.pm/elixir/String.html#downcase/1)
- Dokumentation zu [`String.lowercase/1`](https://hexdocs.pm/elixir/String.html#lowercase/1)
- Erlernung von Elixir: [Einführung in Elixir](https://elixir-lang.org/getting-started/introduction.html)