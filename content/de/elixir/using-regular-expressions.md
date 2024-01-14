---
title:                "Elixir: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Warum

Regular Expressions (kurz RegEx) sind ein unverzichtbares Werkzeug für die Textverarbeitung in der Programmierung. Sie ermöglichen es uns, komplexe Textmuster zu identifizieren und zu manipulieren. In Elixir können wir RegEx sowohl in Strings als auch in Binärdaten verwenden.

# Wie geht's

Um RegEx in Elixir zu nutzen, können wir das `Regex`-Modul verwenden. Hier ist ein Beispiel für eine RegEx, die prüft, ob ein String eine gültige Email-Adresse enthält:

```Elixir
Regex.match?("meine@emailadresse.com", ~r/[\w.]+@[a-z]+\.[a-z]+/)
```
Die Ausgabe würde `true` sein, da die übergebene Email-Adresse dem Muster entspricht. Hier sind einige weitere nützliche Funktionen des `Regex`-Moduls:

- `Regex.match?/2` prüft, ob ein String dem übergebenen Muster entspricht.
- `Regex.replace/3` ersetzt alle in einem String gefundenen Muster durch den angegebenen Wert.
- `Regex.named_captures/2` gibt ein Map zurück, das die benannten Captures (Teilmuster) des Musters und deren Werte enthält.

Weitere Beispiele und Erklärungen findest du in der offiziellen [Elixir-Dokumentation zum `Regex`-Modul](https://hexdocs.pm/elixir/Regex.html).

# Deep Dive

Ein interessantes Feature von RegEx in Elixir ist die Möglichkeit, benannte Captures zu erstellen. Diese werden durch `(?<capture_name>erweiterter_regulärer_ausdruck)` definiert und können dann mit `Regex.named_captures/2` abgerufen werden. Hier ist ein Beispiel, das das Einfügen von benannten Captures in ein String-Format verdeutlicht:

```Elixir
Regex.replace("Mein Name ist {{first_name}} {{last_name}}. Ich bin {{age}} Jahre alt.", 
~r/{{(?<capture>.+)}}/,
%{"first_name" => "Max", "last_name" => "Mustermann", "age" => 30})
```

Die Ausgabe wäre dann `"Mein Name ist Max Mustermann. Ich bin 30 Jahre alt."`. Durch die Verwendung von benannten Captures können wir dynamisch Strings generieren, die auf bestimmte Werte angepasst sind.

# Siehe auch

- [Elixir-Dokumentation zum `Regex`-Modul](https://hexdocs.pm/elixir/Regex.html)
- [Reguläre Ausdrücke lernen - Einführung von Coding Train (Englisch)](https://www.youtube.com/watch?v=r6I-Ahc0HB4)
- [RegExr - interaktiver RegEx-Tester und Referenz (Englisch)](https://regexr.com/)