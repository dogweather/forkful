---
title:                "Suchen und Ersetzen von Texten"
html_title:           "Elixir: Suchen und Ersetzen von Texten"
simple_title:         "Suchen und Ersetzen von Texten"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum
Wenn du mit Texten arbeitest, sei es in einer Datei oder in einer Datenbank, kommst du hin und wieder an den Punkt, an dem du bestimmte Wörter oder Zeichen ersetzen möchtest. Um diesen Prozess effizienter und schneller zu gestalten, ist die Nutzung von Suchen-und-Ersetzen-Funktionen unerlässlich.

## Wie es funktioniert
Suchen und Ersetzen kann in Elixir mithilfe des `String.replace/3`-Befehls durchgeführt werden. Wie der Name schon sagt, ersetzt dieser Befehl einen Teil eines Strings durch einen anderen. Hier ist ein Beispiel:

```elixir
str = "Guten Abend, wie geht es Ihnen?"
String.replace(str, "Abend", "Morgen")
```
Output: "Guten Morgen, wie geht es Ihnen?"

In diesem Beispiel wurde das Wort "Abend" durch "Morgen" ersetzt. Beachte, dass der Befehl `String.replace/3` immer einen neuen String zurückgibt und den ursprünglichen String nicht verändert.

Zusätzlich zu einfachen Zeichenketten können auch reguläre Ausdrücke verwendet werden, um Text zu suchen und zu ersetzen. Dazu wird die Funktion `Regex.replace/3` genutzt. Hier ist ein Beispiel:

```elixir
str = "Heute ist der 10. Juli."
Regex.replace(~r/\d+/, str, "11.")
```
Output: "Heute ist der 11. Juli."

In diesem Beispiel wurde die Zahl 10 durch 11 ersetzt. Beachte, dass der reguläre Ausdruck `~r/\d+/` für eine beliebige Zahl steht.

## Tiefergehende Informationen
Es gibt noch viele weitere Funktionen und Optionen, die beim Suchen und Ersetzen von Text in Elixir zur Verfügung stehen. Dazu gehören unter anderem die Möglichkeit, nur in einem bestimmten Teil eines Strings zu suchen und zu ersetzen oder den Befehl mehrmals auf den gleichen String anzuwenden. Du kannst dich mit diesen Funktionen näher vertraut machen, indem du die Dokumentation von Elixir zu `String.replace/3` und `Regex.replace/3` konsultierst.

## Siehe auch
- [Elixir Dokumentation - String.replace/3](https://hexdocs.pm/elixir/String.html#replace/3)
- [Elixir Dokumentation - Regex.replace/3](https://hexdocs.pm/elixir/String.html#replace/3)