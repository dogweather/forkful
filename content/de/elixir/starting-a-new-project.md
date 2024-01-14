---
title:    "Elixir: Eine neue Programmierprojekt beginnen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum

Elixir ist eine aufstrebende Programmiersprache, die immer mehr Fans gewinnt. Sie zeichnet sich durch eine einfache Syntax, Skalierbarkeit und Zuverlässigkeit aus. Wenn du nach einer neuen Herausforderung suchst oder einfach nur deine Skills erweitern möchtest, könnte es Zeit sein, ein neues Projekt mit Elixir zu starten.

## Wie man anfängt

Als erstes solltest du Elixir auf deinem Computer installieren. Dazu kannst du den offiziellen Guide auf der [Elixir-Website](https://elixir-lang.org/install.html) verwenden.

Sobald du Elixir installiert hast, kannst du ein neues Projekt erstellen, indem du den Befehl `mix new project_name` ausführst. Dieser Befehl erstellt eine Grundstruktur für dein Projekt und erstellt auch eine `mix.exs` Datei, in der du Abhängigkeiten angeben kannst.

Nun kannst du deinen Code in der `lib` Ordner schreiben und die Tests in der `test` Ordner ausführen. Du kannst die Tests mit dem Befehl `mix test` ausführen.

Um dein Projekt zu kompilieren und auszuführen, kannst du `iex -S mix` verwenden. Dies öffnet eine interaktive Konsole, in der du Codezeilen testen und ausführen kannst.

### Beispiel

```Elixir
defmodule Hello do
  def print() do
    IO.puts "Hallo Welt!"
  end
end
```

Dies ist ein einfaches Modul, das eine `print` Funktion enthält, die "Hallo Welt!" ausgibt. Um dies in der interaktiven Konsole auszuführen, können wir folgendes machen:

```Elixir
iex> Hello.print()
Hallo Welt!
:ok
```

## Deep Dive

Wenn du tiefer in die Welt von Elixir eintauchen möchtest, gibt es noch viele weitere Funktionen und Konzepte, die es zu entdecken gibt. Eine wichtige Funktion von Elixir sind zum Beispiel die Pattern Matching Möglichkeiten. Auch das Konzept der Funktionspipelines ist einzigartig und ermöglicht es, komplexe Funktionalitäten auf einfache Weise zu kombinieren.

Weiterhin gibt es in Elixir eine starke Unterstützung für parallele und nebenläufige Programmierung, was es zu einer guten Wahl für skalierbare Projekte macht.

Ein weiterer Vorteil von Elixir ist die Verwendung der Virtual Machine Erlang, die für ihre Stabilität und Fehlertoleranz bekannt ist.

## Siehe auch

- [Elixir offizielle Website](https://elixir-lang.org/)
- [Elixir GitHub Repository](https://github.com/elixir-lang/elixir)
- [Elixir Forum](https://elixirforum.com/)

Wir hoffen, dieser Blogbeitrag hat dir einen Einblick in die Grundlagen von Elixir gegeben und dich dazu inspiriert, ein neues Projekt mit dieser aufstrebenden Programmiersprache zu starten. Viel Spaß beim Coden!