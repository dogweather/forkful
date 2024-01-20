---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Länge eines Strings zu finden ist die Methode, mit der wir die Anzahl der Zeichen in einer bestimmten  Sequenz ermitteln. Programmierer tun dies, um unterstützende Funktionen wie die Textbearbeitung zu ermöglichen und um die Datenvalidierung sicherzustellen.

## Wie macht man das:

In Elixir verwenden wir die Funktion `String.length()` um die Länge eines Strings zu ermitteln.

```Elixir
iex> String.length("Guten Tag!")
10
```
In dem gegebenen Beispiel liefert `String.length("Guten Tag!")` den Wert 10, was die Länge des Strings "Guten Tag!" ist.

## Tiefere Einblicke

Die Methode zur Ermittlung der Länge eines Strings hat ihren Ursprung in frühen Programmiersprachen und ist auch heute noch in den meisten hochrangigen Sprachen vorhanden. Alternativen zur `String.length()` Funktion in Elixir sind eher selten, da sie eine effiziente und genaue Methode zur Verfügung stellt. Technisch gesehen, zählt `length()` in Elixir nicht Zeichen, sondern Grapheme, was bedeutet, dass es korrekt funktionieren wird, auch wenn der Text Unicode-Zeichen enthält.

## Siehe auch:

Für mehr Formatierungsmethoden in Elixir, besuchen Sie die [Elixir Dokumentation](https://hexdocs.pm/elixir/String.html). Sie möchten sich auf andere Funktionen konzentrieren? Schauen Sie sich das [Elixir School](https://elixirschool.com/de/) an - eine großartige Ressource für das Lernen von Elixir.