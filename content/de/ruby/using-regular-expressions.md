---
title:    "Ruby: Verwendung von regulären Ausdrücken"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Viele Programmierer verwenden reguläre Ausdrücke, um Text zu analysieren und zu manipulieren. Sie können dabei helfen, komplexe Muster in Texten zu finden und zu ersetzen. Darüber hinaus sind sie eine effektive Möglichkeit, Daten zu validieren und zu überprüfen.

## Anleitung

Um reguläre Ausdrücke in Ruby zu verwenden, müssen wir zuerst das Modul `Regex` importieren. Dann können wir den `=~` Operator verwenden, um einen regulären Ausdruck auf einen String anzuwenden. Hier ist ein Beispiel, das prüft, ob eine E-Mail-Adresse gültig ist:

```Ruby
require 'regex'

email = 'test@test.com'
if email =~ /\A[\w+\-.]+@[a-z\d\-]+(\.[a-z]+)*\.[a-z]+\z/i
  puts 'Diese E-Mail-Adresse ist gültig.'
else
  puts 'Diese E-Mail-Adresse ist ungültig.'
end
```

Die Ausgabe ist "Diese E-Mail-Adresse ist gültig." Da die E-Mail-Adresse dem regulären Ausdruck entspricht.

## Tiefere Einblicke

Reguläre Ausdrücke sind sehr leistungsstark und können komplexe Muster abbilden. Sie können Buchstaben, Zahlen, Sonderzeichen und sogar Platzhalter enthalten, um die Suche noch genauer zu machen. Zum Beispiel können wir mit dem `scan`-Methode alle Wörter in einem String finden, die mit einem bestimmten Buchstaben beginnen:

```Ruby
paragraph = 'Reguläre Ausdrücke sind sehr mächtig und eine große Hilfe bei der Verarbeitung von Text.'

words_starting_with_r = paragraph.scan(/\br\w+/)

puts words_starting_with_r # Ausgabe: ["Reguläre", "Regexp", "Regeln", "Regelwerk", "Regierungen"]
```

Wie Sie sehen können, kann die Verwendung von regulären Ausdrücken viel Zeit und Mühe sparen, da sie komplexe Aufgaben schnell und effektiv erledigen können.

## Siehe auch

- [Ruby-Dokumentation zu regulären Ausdrücken](https://docs.ruby-lang.org/en/master/syntax/literals_rdoc.html#label-Regular+Expression)
- [RegExr: Interaktives Tool zum Testen von Regulären Ausdrücken](https://regexr.com/)
- [RegEx-Bibliothek für Ruby](https://rubular.com/)