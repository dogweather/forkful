---
title:                "Die Länge einer Zeichenkette finden"
html_title:           "Elixir: Die Länge einer Zeichenkette finden"
simple_title:         "Die Länge einer Zeichenkette finden"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum
Eines der grundlegenden Konzepte in der Programmierung ist das Arbeiten mit Strings - also Folgen von Zeichen. Oftmals muss man die Länge eines Strings ermitteln und genau das lernen wir in diesem Artikel.

## Wie geht man vor?
In Elixir gibt es mehrere Möglichkeiten, die Länge eines Strings zu finden. Wir werden hier drei verschiedene Methoden vorstellen, die alle dasselbe Resultat liefern. 

```Elixir
# Methode 1: String.length/1

iex> String.length("Hallo")
5
```

Hier verwenden wir die Funktion `String.length/1`, die als Argument einen String erhält und die Länge des Strings zurückgibt. Diese Methode ist die einfachste und intuitivste.

```Elixir
# Methode 2: Enum.count/1

iex> "Hello" |> Enum.count()
5
```

Eine weitere Möglichkeit ist die Verwendung der Funktion `Enum.count/1`, die auf einer beliebigen Sequenz von Elementen (hier: auf einem String) angewendet werden kann. Sie gibt die Anzahl der Elemente in der Sequenz zurück, also in unserem Fall die Länge des Strings.

```Elixir
# Methode 3: Regex.count_match/2

iex> Regex.count_match("Hello", ~r/./)
5
```

Die dritte Methode verwendet eine reguläre Ausdrucksmethode, um die Anzahl an Vorkommen von Zeichen im String zu zählen. Dabei wird der reguläre Ausdruck `.` verwendet, der für jedes Zeichen steht. Dies führt zu einer Zählung aller Zeichen im String und somit zur Ermittlung der Länge.

## Tiefer eintauchen
Hinter den Kulissen verwendet Elixir die gleiche Methode wie Methode 1 und 2 beim Arbeiten mit Strings, nämlich die Funktion `String.length/1`. Sie ist Teil des Elixir-Kernels und nutzt die eingebaute Funktion des Erlang Virtual Machine, um die Länge eines Strings zu bestimmen. 

Es gibt jedoch einen wichtigen Unterschied zwischen den drei Methoden, und das ist die Art und Weise, wie sie mit sogenannten graphematischen Clustern umgehen. Graphematische Cluster sind Kombinationen von Zeichen, die zusammen ein einzelnes visuelles Zeichen bilden. Das bedeutet, dass z.B. "Å" als ein graphematisches Cluster gezählt wird, obwohl es technisch gesehen aus zwei Unicode-Zeichen besteht. 

Die Methoden 1 und 2 behandeln graphematische Cluster wie einzelne Zeichen und liefern somit die erwartete Ausgabe. Methode 3, die reguläre Ausdrücke verwendet, hingegen zählt jedes einzelne Unicode-Zeichen im Cluster und kann somit zu einem anderen Ergebnis führen. Es ist wichtig, sich dessen bewusst zu sein, wenn man mit Strings und deren Länge in Elixir arbeitet.

## Siehe auch
- Dokumentation der Funktion `String.length/1`: https://hexdocs.pm/elixir/String.html#length/1
- Weitere Möglichkeiten, Strings in Elixir zu manipulieren: https://medium.com/@andr00z/string-manipulation-in-elixir-4f8772f6e971
- Unterschied zwischen graphematischen Clustern und Unicode-Zeichen: https://en.wikipedia.org/wiki/Grapheme_cluster