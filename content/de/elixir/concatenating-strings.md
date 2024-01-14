---
title:    "Elixir: Verkettung von Zeichenfolgen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Konkatenieren von Strings ist eine grundlegende Aufgabe in der Elixir Programmierung. Es erlaubt uns, mehrere Strings miteinander zu verbinden, um komplexe Texte oder Ausgaben zu erstellen.

## Wie

Um Strings in Elixir zu verbinden, verwenden wir den Operator `<>`. Schauen wir uns ein Beispiel an:

```Elixir
"Hello " <> "World"
```

Dies würde die Ausgabe `Hello World` erzeugen.

Wir können auch Variablen verwenden, um Strings zu konkatenieren. Schauen wir uns ein weiteres Beispiel an:

```Elixir
greeting = "Hi"
name = "Bob"
```

```Elixir
greeting <> " " <> name
```

Dies würde die Ausgabe `Hi Bob` erzeugen.

Wenn wir mehr als zwei Strings konkatenieren möchten, können wir die `<>`-Operatoren hintereinander schreiben, wie im folgenden Beispiel gezeigt:

```Elixir
"Hello" <> " " <> "World" <> "!" 
```

Dies würde die Ausgabe `Hello World!` erzeugen.

## Deep Dive

Nun, da wir wissen, wie man einfache Strings verbindet, können wir einen Blick auf ein paar erweiterte Konzepte werfen.

### Der `<>` Operator und andere Datentypen

Der `<>` Operator kann nicht nur Strings verbinden, sondern auch mit anderen Datentypen wie Zahlen arbeiten. Dies ermöglicht es uns, dynamische Ausgaben zu erstellen, die Variablen mit anderen Datentypen enthalten.

```Elixir
"1" <> 2
```

Dies würde die Ausgabe `12` erzeugen, da die Zahl 2 automatisch in einen String umgewandelt wird.

### Strings und Erlang-Funktionen

In Elixir können wir auch Funktionen aus der zugrunde liegenden Erlang-Sprache verwenden. Wenn wir z.B. einen String in Großbuchstaben konvertieren möchten, können wir die `:string.toupper/1` Funktion nutzen.

```Elixir
string = "hello"
```

```Elixir
:string.toupper(string)
```

Dies würde die Ausgabe `HELLO` erzeugen.

## Siehe auch

- [Elixir Dokumentation](https://elixir-lang.org/docs.html)
- [Elixir String Modul](https://hexdocs.pm/elixir/String.html)
- [Erlang String Funktionen](http://erlang.org/doc/man/string.html)