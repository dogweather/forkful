---
title:    "Elixir: Schreiben auf Standardfehler"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben auf die Standardfehlerausgabe ist eine wichtige Technik in der Elixir-Programmierung. Es ermöglicht uns, Fehlermeldungen und andere wichtige Informationen während der Ausführung unseres Codes zu sehen. Dies kann uns helfen, Fehler schnell zu erkennen und zu beheben.

## Wie es geht

Um etwas auf die Standardfehlerausgabe zu schreiben, können wir die `IO.puts/2`-Funktion verwenden und als ersten Parameter den Stream `:stderr` angeben. Zum Beispiel:

```Elixir
IO.puts(:stderr, "Das ist ein Beispiel für die Standardfehlerausgabe.")
```

Dies wird die angegebene Nachricht auf die Standardfehlerausgabe schreiben, die oft als roter Text in der Konsole angezeigt wird. Wir können auch Variablen anstatt von harten Coded-Text verwenden, um mehr Kontrolle über unsere Ausgabe zu haben. Zum Beispiel:

```Elixir
error = "Divisionsfehler"
IO.puts(:stderr, "Ein #{error} ist aufgetreten.")
```

Dies wird eine Meldung wie "Ein Divisionsfehler ist aufgetreten." auf die Standardfehlerausgabe schreiben.

## Tiefergehende Informationen

Neben der `IO.puts/2`-Funktion gibt es auch die `IO.write/2`-Funktion, die verwendet werden kann, um Rohdaten auf die Standardfehlerausgabe zu schreiben. Diese Funktion ist nützlich für die Ausgabe von Fehlerdetails oder anderen speziellen Daten, die in einem bestimmten Format dargestellt werden müssen.

Es ist auch möglich, mit dem `IO.ANSI`-Modul die Farbe der Ausgabe auf der Standardfehlerausgabe zu ändern. Dies kann helfen, wichtige Informationen hervorzuheben und das Lesen von Fehlermeldungen einfacher zu machen.

## Siehe auch

Hier sind einige Links, die Ihnen helfen können, mehr über das Schreiben auf die Standardfehlerausgabe in Elixir zu erfahren:

- Offizielle Dokumentation für die `IO`-Module: https://hexdocs.pm/elixir/IO.html
- Erlang-Referenz für das `:stderr`-Modul: http://erlang.org/doc/man/tty.html#out_3
- Einleitung in ANSI-Terminalcodes: https://en.wikipedia.org/wiki/ANSI_escape_code