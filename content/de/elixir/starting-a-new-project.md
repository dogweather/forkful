---
title:                "Ein neues Projekt starten"
html_title:           "C#: Ein neues Projekt starten"
simple_title:         "Ein neues Projekt starten"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein neues Projekt anzufangen bedeutet, von Grund auf etwas Neues zu erstellen. Programmeure tun dies, um maßgeschneiderte Lösungen für spezifische Anforderungen zu entwickeln.

## So geht's:

Um ein neues Elixir-Projekt zu starten, verwenden wir `mix`. Hier ist ein Beispiel:

```elixir
# Starten Sie Ihr Terminal und geben Sie folgendes ein:
mix new mein_projekt
# Dies erzeugt ein neues Projekt mit dem Namen 'mein_projekt'
```

Nachdem Sie diese Befehle ausgeführt haben, sehen Sie Folgendes:

```elixir
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating config
* creating config/config.exs
* creating lib
* creating lib/mein_projekt.ex
* creating test
* creating test/test_helper.exs
* creating test/mein_projekt_test.exs
```

Dies zeigt, dass `mix` eine Projektstruktur für Sie erstellt.

## Tiefer Eintauchen:

`mix` ist ein Build-Tool, das mit Elixir eingeführt wurde 2012. Es hat ein einfaches Design und ist doch sehr mächtig. Alternativen zu `mix` könnten andere Build-Tools wie `Maven` oder `Gradle` in der Java-Welt sein. 

Die Implementierungsdetails des `mix new` Befehls belaufen sich auf die Erstellung einer vollständigen, getesteten Projektstruktur, bei der alle nötigen Dateien und Verzeichnisse antizipiert und strukturiert erstellt werden. Es erspart Manuallaufarbeit und ermöglicht es Ihnen, sofort mit dem tatsächlichen Codieren zu beginnen.

## Siehe Auch:

- [Elixir Offizielle Dokumentation](https://elixir-lang.org/docs.html)
- [Mix Build Tool Dokumentation](https://hexdocs.pm/mix/Mix.html)
- [Elixirs "Getting Started" Guide](https://elixir-lang.org/getting-started/introduction.html)