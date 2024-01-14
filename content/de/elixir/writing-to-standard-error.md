---
title:                "Elixir: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben auf die Standardfehlerausgabe ist von großer Bedeutung, um Fehler und Warnungen in unserem Elixir-Code zu identifizieren und zu beheben.

## Wie geht das?

Das Schreiben auf die Standardfehlerausgabe ist einfach und unerlässlich für jeden Elixir-Entwickler. Hier ist ein Beispiel, wie man einen Wert auf die Fehlerausgabe schreibt:

```Elixir
IO.puts(:stderr, "Dies ist ein Beispiel für die Standardfehlerausgabe!")
```

Das obige Beispiel wird den Text "Dies ist ein Beispiel für die Standardfehlerausgabe!" auf die Standardfehlerausgabe schreiben. Hier ist ein weiteres Beispiel, diesmal mit einem Erlang-Modul:

```Elixir
:io.format(:stderr, "Das ist ein Beispiel aus dem Modul ~p", [ErlangModule])
```

Dies wird den Namen des Moduls "ErlangModule" auf die Standardfehlerausgabe schreiben.

## Eintauchen in die Tiefe

Das Schreiben auf die Standardfehlerausgabe kann auch verwendet werden, um zusätzliche Informationen während der Ausführung unseres Codes zu verfolgen. Dies kann besonders hilfreich sein, wenn wir mit größeren und komplexeren Code-Basen arbeiten. Wir können verschiedene Debugging-Informationen wie z.B. die Werte von Variablen oder Funktionsaufrufe auf die Standardfehlerausgabe schreiben, um unser Programm zu überwachen und potenzielle Fehlerquellen zu finden.

Ein weiterer Vorteil des Schreibens auf die Standardfehlerausgabe ist, dass die Daten in Echtzeit angezeigt werden. Im Gegensatz zur Standardausgabe werden die Daten auf der Standardfehlerausgabe ohne Pufferung direkt ausgegeben.

Um effektiv auf die Standardfehlerausgabe zu schreiben, sollten wir uns mit der Dokumentation der Elixir-'IO' und Erlang-'io'-Module vertraut machen.

## Siehe auch
- [Offizielle Elixir-Dokumentation über das Schreiben auf die Standardfehlerausgabe](https://hexdocs.pm/elixir/IO.html#puts/2)
- [Offizielle Erlang-Dokumentation über das Schreiben auf die Standardfehlerausgabe] (http://erlang.org/doc/man/io.html#format-2)