---
title:    "Elixir: Lesen von Befehlszeilenargumenten"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Warum

In diesem Blog-Beitrag werden wir uns damit beschäftigen, wie wir Befehlszeilenargumente in Elixir lesen können. Dies ist eine wichtige Fähigkeit für jeden Elixir-Entwickler, der robuste und dynamische Anwendungen erstellen möchte.

## Wie geht man vor

Das Lesen von Befehlszeilenargumenten in Elixir ist relativ einfach. Wir müssen lediglich das `System`-Modul importieren und die `argv`-Funktion verwenden, um auf die Argumente zuzugreifen. Schauen wir uns ein Beispiel an:

```Elixir
import System

defmodule CLI do
  def main do
    args = argv()
    IO.inspect args
  end
end
```

Und so sieht die Ausgabe aus, wenn wir unser Modul ausführen und einige Argumente übergeben:

```Elixir
CLI.main("Hello", "world")
```

```Elixir
["Hello", "world"]
```

Wie Sie sehen können, gibt uns `argv` ein Array mit den übergebenen Argumenten zurück. Wenn keine Argumente übergeben werden, wird das Array leer sein.

## Tiefen Einblick

Jetzt wo wir wissen, wie wir Befehlszeilenargumente in Elixir lesen können, lassen Sie uns ein wenig genauer betrachten, wie dies funktioniert. Die `argv`-Funktion verwendet die `:code.get_arg`-Funktion unter der Haube, um auf die Argumente zuzugreifen. Dies bedeutet, dass wir auch zusätzliche Optionen angeben können, wie zum Beispiel die Anzahl der Argumente, die wir erwarten.

Ein Beispiel dafür wäre:

```Elixir
args = argv(count: 2)
```

Dies würde angeben, dass wir genau zwei Argumente erwarten und eine Fehlermeldung auslösen, wenn nicht genug übergeben werden.

## Siehe auch

- [Offizielle Elixir-Dokumentation zu System.argv](https://hexdocs.pm/elixir/System.html#argv/1)
- [Elixir School - Kommandzeilenargumente](https://elixirschool.com/de/lessons/basics/command-line-arguments/)