---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was und Warum?

Zusammenfügen von Zeichenketten (englisch "string concatenation") bedeutet, zwei oder mehrere Zeichenketten zu einer einzigen Zeichenkette zu vereinen. Es ist eine grundlegende Operation in der Programmierung, um dynamische Inhalte zu erstellen, Meldungen zu formatieren oder Daten zu speichern und auszugeben.

## Wie es geht:

In Elixir verwenden wir das `<>` Operator, um Zeichenketten zu verbinden. Hier sind einige Beispiele:

Wir können zwei Zeichenketten so zusammenfügen:

```Elixir
name = "Max"
greeting = "Hallo, " <> name
IO.puts greeting
```
Dies wird auf der Konsole ausgeben:
`Hallo, Max`

Wenn wir Inhalte dynamisch generieren, sieht das zum Beispiel so aus:

```Elixir
count = 3
message = "Sie haben " <> Integer.to_string(count) <> " neue Nachrichten."
IO.puts message
```
Dies gibt aus: `Sie haben 3 neue Nachrichten.`

## Tiefer einsteigen:

Historisch gesehen gibt es in vielen Programmiersprachen Methoden, um Zeichenketten zu verbinden. In früheren Versionen von Elixir verknüpften wir Zeichenketten mit der plus-Operator, aber seit Version 1.0 verwenden wir `<>` dafür.

Es gibt auch alternative Möglichkeiten, um Zeichenketten in Elixir zu verbinden, etwa mit Interpolation:

```Elixir
count = 3
message = "Sie haben #{count} neue Nachrichten."
IO.puts message
```

Das vereinfacht oft den Code und macht ihn leichter lesbar, ist aber nicht immer die beste Lösche - etwa dann, wenn Performance in Spiel ist, da `<>` schneller als die Interpolation sein kann.

## Siehe auch:

Für weiterführende Lektüren, schauen Sie bitte die offizielle Elixir Dokumentation hier: https://hexdocs.pm/elixir/String.html. Es hat viele Informationen und Beispiele für die Arbeit mit Zeichenketten und andere Datenformate in Elixir.