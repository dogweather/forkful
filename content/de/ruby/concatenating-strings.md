---
title:    "Ruby: Zusammenfügen von Zeichenketten"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Stellen Sie sich vor, Sie wollen einen kurzen Text in Ihrem Programm erstellen, der aus mehreren einzelnen Wörtern oder Sätzen besteht. Anstatt jede einzelne Zeile zu schreiben, können Sie diese einfach miteinander verbinden. Das nennt man auch "String-Konkatenation". 

## Wie geht's

Es gibt verschiedene Möglichkeiten, Strings in Ruby zu konkatenieren. Die einfachste Methode ist die Verwendung des `+` Operators:

```Ruby
"Hello" + " " + "World" 
```

Das Ergebnis dieser Zeile wäre "Hello World". Sie können auch den `concat` Befehl verwenden:

```Ruby
"Hello".concat(" ").concat("World") 
```
Die Ausgabe wäre ebenfalls "Hello World".

Eine weitere Möglichkeit ist die Verwendung des `<<` Operators, um Strings zu einer bestehenden Variable hinzuzufügen:

```Ruby
word = "World"
"Hello" << " " << word
```

Die Ausgabe wäre auch hier "Hello World".

## Tiefer Einblick

Wichtig ist, dass bei der String-Konkatenation die Reihenfolge der Wörter oder Sätze beachtet wird. Wenn Sie beispielsweise ein Leerzeichen am Anfang oder Ende vergessen, wird dies nicht automatisch hinzugefügt. Hier müssen Sie manuell darauf achten.

Außerdem können Sie auch Variablen und andere Datentypen in die Konkatenation einbeziehen. Zum Beispiel:

```Ruby
name = "Max"
age = 25
"Hello, my name is " + name + " and I am " + age.to_s + " years old."
```

Die Ausgabe wäre dann "Hello, my name is Max and I am 25 years old." Wie Sie sehen, müssen wir die Zahl `age` mit `.to_s` in einen String umwandeln, damit sie mit den anderen Strings konkateniert werden kann.

## Siehe auch

Hier sind einige hilfreiche Links, die Sie bei der weiteren Lernphase unterstützen können:

- [Offizielle Ruby-Dokumentation zur String-Konkatenation](https://ruby-doc.org/core-3.0.2/String.html#method-i-2B)
- [Tutorial zur Ruby-Syntax](https://www.ruby-lang.org/de/documentation/quickstart/)
- [Beispiele für String-Konkatenation in Ruby](https://www.rubyguides.com/2019/03/ruby-string-concatenation/)