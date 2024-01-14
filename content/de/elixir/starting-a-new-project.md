---
title:    "Elixir: Ein neues Projekt beginnen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Warum

Du hast vielleicht davon gehört, dass Elixir eine aufstrebende Programmiersprache ist, die auf der bewährten Erlang-Plattform aufbaut. Aber warum sollte man sich die Mühe machen, ein neues Projekt mit Elixir zu starten? Nun, es gibt mehrere gute Gründe dafür:

- Elixir ist eine funktionale Sprache, die es dir ermöglicht, eleganten und skalierbaren Code zu schreiben.
- Durch die Verwendung der Erlang-Plattform profitiert Elixir von der Stabilität und Robustheit, für die Erlang bekannt ist.
- Elixir ist eine Sprache, die für parallele und verteilte Systeme entwickelt wurde, was sie perfekt für die Entwicklung von robusten Back-End-Anwendungen macht.
- Die wachsende Community und die Unterstützung durch Unternehmen wie WhatsApp und Pinterest machen Elixir zu einer vielversprechenden Sprache für die Zukunft.

Wenn diese Gründe dich überzeugt haben, dann lass uns jetzt darüber sprechen, wie du mit Elixir loslegen kannst.

## How To

Um ein neues Projekt mit Elixir zu starten, musst du zuerst Elixir und die zugehörige Build-Tool-Kette installieren. Dies ist am besten über den Paket-Manager deiner Wahl möglich. Wenn du beispielsweise Nix verwendest, kannst du einfach `nix-shell -p elixir` ausführen.

Sobald du Elixir installiert hast, kannst du ein neues Projekt mit `mix new projectname` erstellen. Dies wird ein neues Projektverzeichnis mit einer grundlegenden Ordnerstruktur erstellen.

Jetzt kannst du deine Code-Beispiele in Elixir schreiben, indem du einfach den Code in einem "```Elixir ... ```" Code-Block umschließt:

```Elixir
defmodule HelloWorld do
  def hello do
    IO.puts "Hallo Welt!"
  end
end

HelloWorld.hello
```

Dieses einfache Beispiel zeigt, wie man ein Modul in Elixir definiert und eine Funktion in diesem Modul aufruft.

## Deep Dive

Nun, da du eine Grundidee hast, wie man mit Elixir Code schreibt, wollen wir uns etwas tiefer in die Sprache eintauchen. Elixir bietet einige interessante Konzepte, die es zu entdecken lohnt. Ein Beispiel ist die Möglichkeit, Musterabgleiche (pattern matching) für Variablenzuweisungen zu verwenden:

```Elixir
defmodule Match do
  def fact(0), do: 1
  def fact(n), do: n * fact(n - 1)
end

Match.fact(5) # gibt 120 zurück
```

Hier sehen wir, wie wir eine Funktion definieren, die den Fakultätswert einer Zahl berechnet. Durch die Verwendung von Musterabgleichen können wir die Basisfälle 0 und 1 direkt in der Funktionsdefinition behandeln, anstatt sie in einer separaten bedingten Anweisung zu überprüfen.

Weitere fortgeschrittene Konzepte in Elixir sind die Benutzung von Makros und die Entwicklung von eigenen Protkollen. Wir werden in zukünftigen Beiträgen darüber sprechen.

## Siehe auch

- Offizielle Elixir-Homepage: https://elixir-lang.org/
- Elixir Dokumentation: https://hexdocs.pm/elixir/
- Elixir Forum: https://elixirforum.com/