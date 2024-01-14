---
title:    "Elixir: Druck von Debug-Ausgabe"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debugging ist ein wichtiger Teil des Programmierens und das Drucken von Debug-Ausgaben kann dabei eine sehr nützliche Methode sein. Es hilft dabei, den Code während der Entwicklung zu verstehen, Fehler zu finden und zu beheben.

## Wie

Es gibt verschiedene Möglichkeiten, um Debug-Ausgaben in Elixir zu drucken. Die einfachste Möglichkeit ist die Verwendung der `IO`-Funktion `inspect/2`, die eine lesbare Darstellung von Datenstrukturen zurückgibt. Zum Beispiel:

```Elixir
IO.inspect(["Hello", "World"])
```

Dies gibt die Ausgabe `"["Hello", "World"]"` zurück. Die Funktion kann auch mit Modulen und Funktionen verwendet werden, um Informationen über diese auszugeben. Zum Beispiel:

```Elixir
IO.inspect(Enum, pretty: true)
```

Dies gibt eine hübsch formatierte Ausgabe aller Funktionen im Modul `Enum` zurück.

Eine weitere nützliche Funktion für das Drucken von Debug-Ausgaben ist `IO.puts/2`, die einfach eine Zeichenkette ohne Formatierung druckt. Zum Beispiel:

```Elixir
IO.puts("This is a debug message")
```

Dies wird einfach die Zeichenkette `"This is a debug message"` ausgeben.

Es ist auch möglich, eigene formatierte Debug-Ausgaben zu erstellen, indem man die `~p`- und `~s`-Formatierungsoptionen verwendet. Zum Beispiel:

```Elixir
name = "John"
age = 25
IO.puts(~p"Name: #{name} - Age: #{age}")
```

Dies wird die Ausgabe `"Name: \"John\" - Age: 25"` erzeugen.

## Tiefeneintauchen

Beim Drucken von Debug-Ausgaben ist es wichtig, zu beachten, dass zu viele Ausgaben den Code unordentlich machen und hemmen können. Daher sollte man genau überlegen, welche Informationen ausgegeben werden sollen. Es ist auch möglich, die Ausgabe von Debug-Ausgaben anhand von Umgebungsvariablen oder Kompilierungsoptionen zu steuern. Zum Beispiel:

```Elixir
if System.get_env("DEBUG") == "true" do
  IO.puts("This is a debug message")
end
```

Dies wird nur dann eine Debug-Ausgabe drucken, wenn die Umgebungsvariable `DEBUG` auf `true` gesetzt ist.

## Siehe auch

- Offizielle Elixir Dokumentation zu Debugging: https://elixir-lang.org/getting-started/debugging.html
- Elixir Forum Diskussion über die Verwendung von Debug-Ausgaben: https://elixirforum.com/t/elixir-forum-diskussion
- Elixir School Tutorial über Debug-Ausgaben: https://elixirschool.com/de/lessons/basics/io-debugger/