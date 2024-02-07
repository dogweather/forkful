---
title:                "Einen neuen Projekt starten"
date:                  2024-01-20T18:03:18.498913-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen neuen Projekt starten"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein neues Projekt zu starten bedeutet, eine frische Codebasis zu initialisieren, in der wir unsere Ideen umsetzen können. Programmierer beginnen neue Projekte, um Lösungen für Probleme zu schaffen, neue Tools zu entwickeln oder einfach, um neue Technologien zu erlernen.

## How to:
Zum Starten eines neuen Elixir-Projekts nutzen wir Mix, Elixirs Build-Tool. Hier ein Beispiel:

```elixir
# Installation von Elixir, falls noch nicht geschehen
mix local.hex

# Erstellung eines neuen Elixir-Projekts
mix new mein_projekt

# Navigieren in das Projektverzeichnis
cd mein_projekt

# Start der interaktiven Elixir-Konsole mit dem Projekt
iex -S mix
```

Ausgabe:

```
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/mein_projekt.ex
* creating test
* creating test/test_helper.exs
* creating test/mein_projekt_test.exs

Your Mix project was created successfully.
You can use mix to compile it, test it, and more:

    cd mein_projekt
    mix test

Run `mix help` for more commands.
```

## Deep Dive
Elixir verwendet Mix, um neue Projekte zu erstellen und zu verwalten. Mix ist seit Elixirs Anfängen Teil der Sprache. Alternativen wie `rebar3` existieren in der Erlang-Welt, doch Mix ist das Standard-Tool in Elixir. Es lädt Abhängigkeiten, kompiliert den Code und führt Tests durch. Projektinformationen und Abhängigkeiten werden in `mix.exs` definiert. Seit Elixir 1.0 ist Mix stabil und wird breit genutzt.

## See Also
- [Elixir Getting Started Guide](https://elixir-lang.org/getting-started/introduction.html)
- Dokumentation von Mix: [https://hexdocs.pm/mix/Mix.html](https://hexdocs.pm/mix/Mix.html)
- Elixir School für Lernressourcen: [https://elixirschool.com/de/](https://elixirschool.com/de/)
