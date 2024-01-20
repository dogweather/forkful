---
title:                "Reguläre Ausdrücke verwenden"
html_title:           "Bash: Reguläre Ausdrücke verwenden"
simple_title:         "Reguläre Ausdrücke verwenden"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?

Reguläre Ausdrücke (Regular Expressions/Regex) sind Muster, mit denen Entwickler Sequenzen in Text suchen, ersetzen oder validieren können. Sie sind ein nützliches und effizientes Werkzeug für Datenmanipulation und Textverarbeitung.

## Wie geht das:

Hier ist, wie Sie reguläre Ausdrücke in Elixir verwenden können. 

Sie können das Regex-Modul in Elixir verwenden, um reguläre Ausdrücke zu kompilieren und damit zu arbeiten. Hier ist ein einfacher Code zum Suchen eines Musters in einer Zeichenkette.

```elixir
String.match?("Hallo, Elixir Welt!", ~r/Elixir/)
```
Die Ausgabe dieses Beispiels wird `true` sein, da das Wort "Elixir" in der Zeichenkette enthalten ist.

## Vertiefung:

Historisch gesehen waren reguläre Ausdrücke in verschiedenen Programmiersprachen vorhanden, und Elixir ist keine Ausnahme. Elixir implementiert seine Regex-Unterstützung über das Erlang :re Modul, das auf dem PCRE (Perl Compatible Regular Expressions) Bibliothek basiert.

Es gibt Alternativen zu regulären Ausdrücken in Elixir, z.B. die Verwendung von eingebauten String-Funktionen wie `String.contains?/2` oder `String.split/2`.

Es ist wichtig zu wissen, dass die Verwendung von regulären Ausdrücken in Elixir bei komplexen Mustern rechenintensiv sein kann, da das Kompilieren eines regulären Ausdrucks eine aufwendige Operation ist.

## Weiterführende Informationen:

Weitere nützliche Ressourcen zu regulären Ausdrücken in Elixir finden Sie unter:

2. [Elixir official Regex module documentation](https://hexdocs.pm/elixir/Regex.html)