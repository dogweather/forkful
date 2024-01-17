---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Elixir: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Was und Warum?
Das Einlesen von Befehlszeilenargumenten ist eine Technik, die es Programmierern ermöglicht, Benutzereingaben direkt in ihrem Code zu verarbeiten. Es ist eine effiziente Möglichkeit, eine interaktive Anwendung zu erstellen oder das Verhalten einer Anwendung anzupassen, ohne den Quellcode ändern zu müssen.

Wie geht's?
Um Befehlszeilenargumente in Elixir zu lesen, können wir die "System.argv" Funktion verwenden. Diese Funktion gibt eine Liste der Argumente zurück, die bei dem Aufruf des Codes angegeben wurden. Wir können sie dann in unserem Code verwenden, um die gewünschten Aktionen auszuführen.

```
Elixir
args = System.argv
IO.puts("Das erste Argument ist: #{args[0]}")
```

Beispieloutput:
```
> elixir argumente.exs argument1 argument2
Das erste Argument ist: argument1
```

Tiefgehende Einblicke
Das Einlesen von Befehlszeilenargumenten ist eine gebräuchliche Technik in vielen Programmiersprachen, einschließlich Elixir. Es ermöglicht es uns, interaktive Anwendungen zu erstellen, ohne dass der Benutzer jedes Mal die gleichen Eingaben tätigen muss. Alternativ können wir auch Umgebungsvariablen verwenden, um Benutzereingaben zu lesen. Einer der Gründe, warum Programmierer Befehlszeilenargumente verwenden, ist die Flexibilität und Anpassbarkeit, die sie bieten.

Siehe auch
- [Official Elixir Documentation for System.argv](https://hexdocs.pm/elixir/System.html#argv/0)
- [Understanding Command Line Arguments in Elixir](https://medium.com/@vgraziano/understanding-command-line-arguments-in-elixir-2ebfe9160f6d)
- [Elixir - Reading Command Line Input](https://learngowith.me/elixir-reading-command-line-input)