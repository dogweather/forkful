---
title:                "Code in Funktionen organisieren"
aliases: - /de/elixir/organizing-code-into-functions.md
date:                  2024-01-26T01:09:42.755256-07:00
model:                 gpt-4-1106-preview
simple_title:         "Code in Funktionen organisieren"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Code in Funktionen zu organisieren bedeutet, verwandte Operationen in wiederverwendbare Blöcke zu gliedern. Wir tun dies, um die Lesbarkeit und Wartbarkeit zu erhöhen, Duplikationen zu reduzieren und das Testen zu vereinfachen.

## Wie geht das:
Lassen Sie uns eine einfache Elixir-Funktion erstellen, um Wörter zu kapitalisieren:

```elixir
defmodule StringUtils do
  def capitalize_words(sentence) do
    sentence
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts StringUtils.capitalize_words("hello elixir world")
```
Ausgabe:
```
Hello Elixir World
```
Hier haben wir die Logik zur Großschreibung von Wörtern ordentlich in eine Funktion namens `capitalize_words` verpackt.

## Tiefere Betrachtung
In Elixir und im weiteren Ökosystem der Erlang VM sind Funktionen Bürger erster Klasse, sie erben die Philosophie, Probleme in kleinere, handhabbare und isolierte Teile zu zerlegen. Historisch gesehen hat dieser funktionale Ansatz seine Wurzeln im Lambda-Kalkül und in Lisps, die die Philosophie von Code als Daten fördern.

Alternativen zur Organisation von Code können in Elixir das Verwenden von Makros oder Prozessen für wiederholende oder gleichzeitige Aufgaben sein. Implementierungstechnisch können Elixir-Funktionen Musterabgleich durchführen und unterschiedliche Argumente (Arity) erhalten, was ihnen Vielseitigkeit verleiht.

## Siehe auch
- [Elixirs offizielle Dokumentation zu Funktionen](https://hexdocs.pm/elixir/Kernel.html#functions)
- [Dave Thomas "Programming Elixir"](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
