---
title:    "Elixir: Das Lesen einer Textdatei"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Warum?

Das Lesen von Textdateien gehört zu den grundlegendsten Funktionen der Programmierung. Es ist wichtig, um Daten zu verarbeiten, zu analysieren und zu manipulieren. Elixir bietet dabei eine einfache und effektive Methode, um Textdateien zu lesen und zu nutzen.

## Wie geht man vor?

Um eine Textdatei in Elixir zu lesen, können die Funktionen `File.open` und `IO.read` verwendet werden. Wir können zunächst eine Datei mit `File.open/2` öffnen und dann den Inhalt mit `IO.read/2` auslesen. Die folgenden Code-Beispiele zeigen, wie man eine Textdatei mit Elixir liest:

```
Elixir Datei öffnen und Inhalt lesen:

File.open("beispiel.txt") |>
IO.read() |>
IO.puts()
```

#### Beispieldatei "beispiel.txt":

```
Dies ist ein Beispieltext. 
In dieser Datei befinden sich einige Zeilen mit Daten.
```

#### Ausgabe:

```
Dies ist ein Beispieltext.
In dieser Datei befinden sich einige Zeilen mit Daten.
```

Hier sehen wir, dass der Inhalt der Datei erfolgreich gelesen und ausgegeben wurde.

## Tiefergehende Informationen

Beim Lesen von Textdateien in Elixir gibt es einige Dinge zu beachten. Die Funktion `IO.read/2` kann verschiedene Optionen enthalten, wie z.B. die Anzahl der Zeichen, die gelesen werden sollen, oder das Encoding der Datei. Ebenfalls wichtig ist, dass die Datei wieder geschlossen wird, um eventuelle Ressourcen freizugeben. Daher sollte man immer die Funktion `File.stream/1` oder `File.stream!/1` verwenden, um eine Datei zu öffnen. Diese Funktionen geben einen Stream zurück, der automatisch geschlossen wird, wenn er die Dateiende erreicht.

Ein weiterer wichtiger Punkt ist, dass Elixir standardmäßig UTF-8 als Encoding verwendet. Wenn die Textdatei in einem anderen Encoding vorliegt, muss dies beim Öffnen der Datei angegeben werden, z.B.: `File.stream!("beispiel.txt", [:encoding, :latin1])`.

## Siehe auch

1) [Elixir Dokumentation: File.open/2](https://hexdocs.pm/elixir/File.html#open/2)
2) [Elixir Dokumentation: IO.read/2](https://hexdocs.pm/elixir/IO.html#read/2)
3) [Elixir Dokumentation: File.stream/1](https://hexdocs.pm/elixir/File.html#stream/1)
4) [Elixir Dokumentation: File.stream!/1](https://hexdocs.pm/elixir/File.html#stream!/1)