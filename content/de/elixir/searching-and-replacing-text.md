---
title:                "Elixir: Suchen und Ersetzen von Text"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Das Suchen und Ersetzen von Text ist eine häufige Aufgabe beim Programmieren. Elixir bietet eine Vielzahl an Funktionen und Methoden, die diese Aufgabe erleichtern. In diesem Blog-Beitrag werden wir uns genauer anschauen, wie man Text in Elixir suchen und ersetzen kann.

## How To

Um Text in Elixir zu suchen und zu ersetzen, gibt es mehrere Optionen. Eine Möglichkeit ist die ```String.replace/3``` Funktion, die Teil des Elixir Standard-Library ist. Diese Funktion akzeptiert drei Argumente: einen Elixir String, ein Muster (wie eine reguläre Ausdruck) und eine Ersetzungszeichenkette. Im folgenden Beispiel suchen wir nach allen Vokalen (a, e, i, o, u) in einem String und ersetzen sie durch das Zeichen "X":

```
iex> String.replace("Hello World", ~r/[aeiou]/, "X")
"HXllX Wxrld"
```

Eine andere Möglichkeit ist die ```Regex.replace/3``` Funktion, die Teil des Elixir Regex-Moduls ist. Diese Funktion funktioniert ähnlich wie ```String.replace/3```, akzeptiert jedoch auch reguläre Ausdrücke als Ersetzungsmuster. Im folgenden Beispiel ersetzen wir alle Vokale durch das Zeichen "X" und konvertieren den ursprünglichen String gleichzeitig in Großbuchstaben:

```
iex> Regex.replace("Hello World", ~r/[aeiou]/, "X", global: true, insert_case: :upper)
"HXLLX WXRLD"
```

Die ```String.replace/3``` und ```Regex.replace/3``` Funktionen sind nur zwei Möglichkeiten, wie man Text in Elixir suchen und ersetzen kann. Es gibt auch andere Funktionen wie ```String.replace_leading/3``` und ```String.replace_trailing/3```, die speziell für das Ersetzen von Teilstrings am Anfang oder Ende eines Strings entwickelt wurden. Es kann hilfreich sein, sich die offizielle Dokumentation anzusehen, um mehr über diese Funktionen und ihre Verwendung zu erfahren.

## Deep Dive

Bei der Suche und dem Ersatz von Text ist es wichtig zu verstehen, wie reguläre Ausdrücke funktionieren. Elixir verwendet dieselbe Syntax für reguläre Ausdrucke wie Perl und Ruby. Reguläre Ausdrücke sind ein leistungsstarkes Werkzeug zum Suchen und Ersetzen von Text in Elixir. Sie können komplexe Muster definieren und ermöglichen es uns, in Strings nach bestimmten Mustern zu suchen, anstatt nur nach expliziten Zeichenketten.

Im obigen Beispiel haben wir den regulären Ausdruck ```~r/[aeiou]/``` verwendet, um nach Vokalen zu suchen. Dieser Ausdruck besteht aus einem sogenannten Charakterklasse ```[aeiou]```, die alle Vokale im Alphabet darstellt. Das vorangestellte Tilde-Zeichen ```~r``` zeigt an, dass es sich um einen regulären Ausdruck handelt, und der nachfolgende Schrägstrich markiert den Anfang und das Ende des Ausdrucks.

Es gibt noch viele weitere wichtige Konzepte und Funktionen, die beim Suchen und Ersetzen von Text in Elixir relevant sind, wie z.B. das Verwenden von Backreferences, das Ignorieren von Groß- und Kleinschreibung und die Verwendung von sogenannten lookaround Operatoren. Für eine tiefere Auseinandersetzung mit regulären Ausdrücken in Elixir empfehle ich die Lektüre des offiziellen Elixir Guides zu diesem Thema.

## Siehe auch

- [Offizielle Elixir Dokumentation](https://hexdocs.pm/elixir/String.html#replace/3)
- [Erfahren Sie mehr über reguläre Ausdrücke in Elixir](https://hexdocs.pm/elixir/Regex.html)