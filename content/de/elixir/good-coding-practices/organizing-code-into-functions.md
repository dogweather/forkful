---
date: 2024-01-26 01:09:42.755256-07:00
description: "Wie geht das: Lassen Sie uns eine einfache Elixir-Funktion erstellen,\
  \ um W\xF6rter zu kapitalisieren."
lastmod: '2024-03-13T22:44:53.539289-06:00'
model: gpt-4-1106-preview
summary: "Lassen Sie uns eine einfache Elixir-Funktion erstellen, um W\xF6rter zu\
  \ kapitalisieren."
title: Code in Funktionen organisieren
weight: 18
---

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
