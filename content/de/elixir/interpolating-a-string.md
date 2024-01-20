---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

String-Interpolation ermöglicht es uns, Variablen direkt in Strings zu setzen. Es ist benutzerfreundlich, liest sich natürlich und hilft, schwer lesbaren Code zu vermeiden.

## So geht's:

In Elixir verwenden wir die `#{}`-Syntax für die String-Interpolation. Schauen wir uns ein Beispiel an.

```elixir
name = "Hans"
IO.puts "Hallo #{name}"
```

Ausgabe wird sein: `Hallo Hans`

## Tiefgehende Untersuchung

String-Interpolation in Elixir ist eine einfache und direkte Art, dynamischen Text zu erstellen. Ursprünglich aus den Sprachen wie Perl und Ruby kommend, ist sie seit Elixir 1.0 ein Kernfeature.

Es gibt Alternativen zur String-Interpolation, wie z.B. die Verwendung von `<>` zum Verketten von Strings, aber diese sind weniger lesbar und intuitiv.

Die Implementierung von Interpolation in Elixir ist einfach und effektiv: der Interpreter ersetzt einfach den `#{}`-Ausdruck durch das Ergebnis seiner Auswertung, konvertiert in einen String.

## Siehe Auch

Für weitere Informationen zur `String`-Interpolation in Elixir, besuchen Sie:

1. Offizielle Elixir-Dokumentation: [https://elixir-lang.org/getting-started/io-and-the-file-system.html#iodots](https://elixir-lang.org/getting-started/io-and-the-file-system.html#iodots)

2. Erlang-Dokumentation (auf der Elixir basiert): [http://erlang.org/doc/man/io.html](http://erlang.org/doc/man/io.html)