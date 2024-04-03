---
date: 2024-01-26 04:12:55.634469-07:00
description: "Wie geht das: Um IEx zu starten, \xF6ffnen Sie Ihr Terminal und geben\
  \ Sie `iex` ein. Hier ist ein Vorgeschmack."
lastmod: '2024-03-13T22:44:53.534738-06:00'
model: gpt-4-0125-preview
summary: "Um IEx zu starten, \xF6ffnen Sie Ihr Terminal und geben Sie `iex` ein."
title: Nutzung einer interaktiven Shell (REPL)
weight: 34
---

## Wie geht das:
Um IEx zu starten, öffnen Sie Ihr Terminal und geben Sie `iex` ein. Hier ist ein Vorgeschmack:

```Elixir
iex> name = "Elixir Programmer"
"Elixir Programmer"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

Die Ausgabe sollte die Zuweisung von Variablen, Funktionsergebnisse und eine anonyme Funktion bei der Arbeit zeigen.

## Tiefergehend
Die IEx-Shell ist seit den Anfangstagen ein Teil von Elixir. José Valim, der Schöpfer von Elixir, ließ sich von den interaktiven Shells anderer Sprachen wie Pythons `python` und Rubys `irb` inspirieren. Obwohl IEx viele Funktionen mit diesen teilt, ist es entwickelt worden, um mit der nebenläufigen Natur von Elixir umzugehen und ist vollständig mit den Fähigkeiten der Erlang VM integriert.

Alternativen zu IEx im Erlang-Ökosystem umfassen `erl`, die Erlang-Shell. Aber IEx bietet eine Elixir-freundlichere Umgebung, mit Funktionen wie umfassender Tab-Vervollständigung, Verlauf und Hilfsprogrammen.

Die IEx-REPL ist mehr als ein Spielplatz; sie kann nahtlos mit einem laufenden System verbunden werden. Dies ist entscheidend für das Debuggen von Live-Anwendungen. Die zugrundeliegende Implementierung stützt sich auf die BEAM (die Erlang VM), was sicherstellt, dass Funktionen wie Hot-Code-Swapping direkt in der Shell unterstützt werden.

## Siehe auch
Schauen Sie sich diese weiterführenden Lektüren und Ressourcen an:

- [Elixir's IEx-Dokumentation](https://hexdocs.pm/iex/IEx.html)
- [Interaktives Elixir (IEx) - Die Elixir-Shell](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Erlangs `erl` Dokumentation](http://erlang.org/doc/man/erl.html)
- [Elixir’s interaktive Shell lernen](https://elixirschool.com/en/lessons/basics/iex_helpers/)
