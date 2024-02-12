---
title:                "Refactoring"
aliases:
- /de/elixir/refactoring/
date:                  2024-01-26T01:17:30.267966-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/refactoring.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Refactoring ist der Prozess der Umstrukturierung bestehenden Codes, ohne dessen externes Verhalten zu ändern. Es zielt darauf ab, nichtfunktionale Attribute wie Lesbarkeit und Wartbarkeit zu verbessern. Programmierer tun dies, um den Code sauberer, leichter verständlich und effizienter zu machen, was zukünftige Updates erleichtert und das Risiko von Fehlern reduziert.

## Wie man:
Lassen Sie uns ein gängiges Elixir-Muster aufräumen. Wir werden eine Funktion `calculate_stats` refaktorisieren, die mehr macht, als sie sollte, indem wir sie in kleinere, wiederverwendbare Teile zerlegen.

```elixir
defmodule Stats do
  # Ursprünglicher, nicht refaktorisierter Code
  def calculate_stats(data) do
    total = Enum.sum(data)
    count = Enum.count(data)
    mean = total / count
    {mean, total}
  end
  
  # Refaktorisierter Code
  def calculate_mean(data), do: Enum.sum(data) / Enum.count(data)
  
  def calculate_total(data), do: Enum.sum(data)
  
  def calculate_stats_refactored(data) do
    mean = calculate_mean(data)
    total = calculate_total(data)
    {mean, total}
  end
end

# Beispiel-Ausgabe
# Vor dem Refactoring
Stats.calculate_stats([1, 2, 3])
# => {2.0, 6}

# Nach dem Refactoring
Stats.calculate_stats_refactored([1, 2, 3])
# => {2.0, 6}
```
Wie Sie sehen können, bleibt die Ausgabe gleich, aber jetzt haben wir modulare Funktionen, die unabhängig voneinander wiederverwendet und getestet werden können.

## Tiefergehende Betrachtung
Refactoring ist kein neues Konzept; es ist seit den Anfängen der Softwareentwicklung ein wesentlicher Teil der Programmierung. Bedeutende Werke, wie Martin Fowlers "Refactoring: Improving the Design of Existing Code", bieten grundlegende Praktiken für das Refactoring und Einblicke in das Wann und Wie ihrer Anwendung.

Alternativen zum manuellen Refactoring umfassen automatisierte Codeanalyse-Tools, die Refactorings vorschlagen oder sogar durchführen können. Allerdings erfassen automatisierte Tools möglicherweise nicht immer den vollen Kontext des Codes und können Feinheiten verpassen, die ein menschlicher Prüfer erkennen würde.

Umsetzungsdetails in Elixir umfassen insbesondere das Verständnis des funktionalen Paradigmas und die Nutzung von Pattern Matching, Guard-Klauseln und dem Pipe-Operator, um klaren und prägnanten Code zu schreiben. Beim Refactoring geht es oft darum, komplexe imperativ-stilisierte Funktionen in kleinere, zusammensetzbare Funktionen umzuwandeln, die Elixirs Vorliebe für Unveränderlichkeit und Seiteneffektfreiheit folgen.

## Siehe auch
Für mehr zu Elixir-spezifischen Refactoring-Techniken:

- [Die offiziellen Guides von Elixir](https://elixir-lang.org/getting-started/)
- [„Refactoring: Improving the Design of Existing Code“ von Martin Fowler](https://martinfowler.com/books/refactoring.html), für allgemeine Prinzipien, die auf Elixir angewendet werden können.
- [Credo, ein statisches Codeanalyse-Tool für Elixir](https://github.com/rrrene/credo), das Best Practices fördert.
- [Exercism Elixir Track](https://exercism.org/tracks/elixir), für praktische Übungen, die oft Refactoring beinhalten.
