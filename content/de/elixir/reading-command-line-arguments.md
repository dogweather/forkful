---
title:                "Elixir: Das Lesen von Befehlszeilen-Argumenten"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum

Wenn du neu in der Welt von Elixir bist, fragst du dich vielleicht, warum du überhaupt command line arguments lesen solltest. Das Lesen von command line arguments ermöglicht es deinen Programmen, Eingaben von Benutzern zu akzeptieren und auf sie zu reagieren. Es ist eine wichtige Fähigkeit, die es dir ermöglicht, interaktive Anwendungen zu entwickeln.

# Wie man es macht

Um command line arguments in Elixir zu lesen, gibt es einige einfache Schritte, die du befolgen musst. Zuerst musst du die Argumente mithilfe der `System.argv` Funktion in eine Liste umwandeln. Du kannst dann durch die Liste iterieren und die Argumente auslesen. Hier ist ein Beispiel des Codes:

```Elixir
defmodule ArgumentReader do
  def read(args) do
    args
    |> Enum.each(fn(arg) -> IO.puts "Argument: #{arg}" end)
  end
end

ArgumentReader.read(System.argv)
```

Wenn wir diesen Code ausführen und als Argumente "hallo" und "welt" angeben, wird folgende Ausgabe erzeugt:

```
Argument: hallo
Argument: welt
```

# Tiefere Einblicke

Du möchtest vielleicht auch wissen, wie du bestimmte Argumente auslesen oder auf sie reagieren kannst. Hier ist ein Beispiel, wie du ein Programm schreiben könntest, das einen Begrüßungstext ausgibt, basierend auf dem ersten Argument:

```Elixir
defmodule ArgumentReader do
  def read(args) do
    case args do
      [_greeting, name] -> IO.puts "Hallo #{name}!"
      _else -> IO.puts "Hallo Welt!"
    end
  end
end

ArgumentReader.read(System.argv)
```

Wenn wir nun den Code mit "Argumente hallo Max" ausführen, wird folgende Ausgabe erzeugt:

```
Hallo Max!
```

Du kannst auch mit komplexeren Argumenten arbeiten, indem du die `System.argc` Funktion verwendest, um die Anzahl der Argumente zu ermitteln und dann entsprechend darauf reagierst. Mit etwas Übung wirst du bald in der Lage sein, komplexe Programme zu schreiben, die auf verschiedene Argumente reagieren können.

# Siehe auch

* [Offizielle Elixir Dokumentation zur System-Modul](https://hexdocs.pm/elixir/System.html)
* [Ausführlicher Artikel über das Verarbeiten von command line arguments](https://www.culttt.com/2016/01/04/handling-command-line-arguments-with-elixir/)