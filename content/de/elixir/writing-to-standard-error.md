---
title:                "Elixir: Schreiben auf Standardfehler"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum
Warum sollte man überhaupt etwas in die Standardfehler-Ausgabe schreiben? Erfahren Sie hier, was der Zweck dieser Funktion ist und wann es sinnvoll ist, sie einzusetzen.

# Wie geht es?
Das Schreiben in die Standardfehler-Ausgabe ist eine grundlegende Funktion in der Elixir-Programmiersprache. Verwenden Sie dazu einfach den Befehl `IO.puts` mit dem Argument `:stderr`:

```elixir
IO.puts("Dies ist eine Fehlermeldung", :stderr)
```

Dies wird die angegebene Nachricht in die Standardfehler-Ausgabe schreiben und sie von regulären Ausgaben unterscheiden. Hier ist ein Beispiel für die Ausgabe in der Konsole:

```elixir
iex> IO.puts("Dies ist eine Fehlermeldung", :stderr)
Dies ist eine Fehlermeldung
:ok
```

# Tiefer in die Materie eintauchen
Es gibt verschiedene Anwendungsfälle, bei denen es sinnvoll ist, in die Standardfehler-Ausgabe zu schreiben. Zum Beispiel kann es nützlich sein, Fehlermeldungen oder Warnungen auszugeben, um Programmierfehler zu identifizieren und zu beheben. Sie können auch benutzerdefinierte Ausgaben erstellen, um Informationen anzuzeigen, die in regulären Ausgaben nicht angezeigt werden sollen.

Es ist auch wichtig zu beachten, dass die Standardfehler-Ausgabe in Elixir asynchron ist, was bedeutet, dass sie nicht in der Reihenfolge angezeigt wird, in der sie geschrieben wurde. Dies kann hilfreich sein, um Fehlermeldungen von regulären Ausgaben zu unterscheiden und die Lesbarkeit zu verbessern.

# Siehe auch
- [Die offizielle Elixir-Dokumentation für `IO.puts`](https://hexdocs.pm/elixir/IO.html#puts/2)
- [Ein Elixir-Tutorial zum Schreiben in die Standardfehler-Ausgabe](https://elixirschool.com/en/lessons/basics/io/)