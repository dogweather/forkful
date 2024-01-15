---
title:                "Suchen und Ersetzen von Text"
html_title:           "Ruby: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Du hast wahrscheinlich schon mal in deinem Alltag eine langweilige Aufgabe wiederholen müssen. Vielleicht hast du mehrere E-Mails geschrieben, die ähnliche Informationen enthielten, oder du hast einen Vertrag bearbeiten müssen, der immer wieder die gleichen Textpassagen enthielt. Mit der Funktion zum Suchen und Ersetzen in Ruby kannst du solche wiederkehrenden Aufgaben automatisieren und sparst dir so viel Zeit und Mühe.

## Wie geht's?

Um Text in Ruby zu suchen und zu ersetzen, verwendest du die Methode `gsub`. Diese nimmt zwei Argumente entgegen: das zu suchende Muster und den Ersatztext. Hier ein Beispiel:

```Ruby
string = "Hello WORLD"
string.gsub("WORLD", "Ruby") # => "Hello Ruby"
```

In diesem Beispiel wird das Wort "WORLD" durch "Ruby" ersetzt. Beachte, dass die Methode `gsub` auf Groß- und Kleinschreibung achtet. Wenn du also "world" ersetzen möchtest, musst du `string.gsub("world", "Ruby")` verwenden. 

Du kannst auch reguläre Ausdrücke verwenden, um noch präziser zu suchen und zu ersetzen. Hier ein Beispiel:

```Ruby
string = "My favorite colors are blue, red, and green"
string.gsub(/b[a-z]+e/, "pink") # => "My favorite colors are pink, red, and pink"
```

In diesem Beispiel wird jede Zeichenfolge ersetzt, die mit "b" beginnt und mit "e" endet, unabhängig von der Länge oder der verwendeten Buchstaben dazwischen. 

## Tief eintauchen

Die Methode `gsub` ist Teil der Ruby-Klasse `String` und verwendet die Klasse `Regexp` für die Suche nach Mustern. Reguläre Ausdrücke sind sehr mächtig und können sehr kompliziert werden. Es lohnt sich daher, sich damit auseinanderzusetzen und sie zu beherrschen, um das Beste aus der Suche und dem Ersetzen von Text in Ruby herausholen zu können.

Weitere Informationen und Beispiele zur Verwendung von `gsub` und regulären Ausdrücken findest du in der offiziellen Dokumentation von Ruby oder auf verschiedenen Online-Plattformen wie [RubyGuides](https://www.rubyguides.com/ruby-strings-substitution/).

## Siehe auch

- [Offizielle Dokumentation von Ruby](https://ruby-doc.org/core-#{RUBY_VERSION}/String.html#method-i-gsub)
- [RubyGuides - Substitution with gsub](https://www.rubyguides.com/ruby-strings-substitution/)
- [Reguläre Ausdrücke in Ruby](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)