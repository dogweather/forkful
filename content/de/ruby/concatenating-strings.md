---
title:                "Verkettung von Zeichenfolgen"
html_title:           "Ruby: Verkettung von Zeichenfolgen"
simple_title:         "Verkettung von Zeichenfolgen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# Warum

Das Verketten von Strings ist eine häufig verwendete Operation in der Ruby-Programmierung. Durch das Zusammenfügen von Strings können wir längere Zeichenfolgen erstellen, die in der Praxis oft benötigt werden.

## Wie

Die einfachste Möglichkeit, Strings in Ruby zu verketten, ist die Verwendung des `+` Operators. Schauen wir uns ein Beispiel an:

```Ruby
name = "Max"
greeting = "Hallo, " + name + "!"
puts greeting
```

Dieser Code wird "Hallo, Max!" ausgeben, da der `+` Operator die Strings "Hallo, " und "Max" zu einem neuen String verbindet. Eine andere Möglichkeit ist die Verwendung des `<<` Operators:

```Ruby
name = "Max"
greeting = "Hallo, "
greeting << name << "!"
puts greeting
```

Dieser Code hat das gleiche Ergebnis wie zuvor, aber anstelle eines neuen Strings, wird dem ursprünglichen String "Hallo, " der Text "Max" und "!" hinzugefügt.

Eine weitere Möglichkeit zur Verkettung von Strings ist die Verwendung der `concat` Methode:

```Ruby
greeting = "Hallo, "
greeting.concat("Max") << "!"
puts greeting
```

Diese Methode wird der bestehenden Zeichenfolge "Hallo, " den Text "Max" anhängen und dann den `<<` Operator verwenden, um "!" an den String anzufügen.

## Deep Dive

Das Verketten von Strings mag auf den ersten Blick einfach erscheinen, aber es gibt einige wichtige Dinge zu beachten. Zum Beispiel kann die Verwendung des `+` Operators sehr ineffizient sein, da er bei jedem Aufruf einen neuen String erstellt. In Situationen, in denen wir viele Strings verketten müssen, ist es daher besser, die `<<` Methode oder die `concat` Methode zu verwenden, um Performance-Probleme zu vermeiden.

Es ist auch wichtig zu wissen, dass Ruby automatisch die `to_s` Methode auf Objekte aufruft, die nicht vom Typ String sind, wenn sie mit dem `+` Operator verwendet werden. Dies kann zu unerwarteten Ergebnissen führen und sollte vermieden werden. Im Zweifelsfall ist es immer am besten, die `to_s` Methode explizit aufzurufen, um sicherzustellen, dass der Code wie gewünscht funktioniert.

Schließlich sollte beachtet werden, dass das Verketten von Strings in Ruby immutable Strings erzeugt. Das bedeutet, dass der ursprüngliche String nicht verändert wird, sondern ein neuer String erstellt wird. Dies kann Auswirkungen auf die Performance haben, besonders wenn wir große Strings verketten.

## Siehe auch

- [Ruby Strings Dokumentation](https://ruby-doc.org/core-2.7.2/String.html)
- [Ruby String Encoding](https://ruby-doc.org/core-2.7.2/Encoding.html)
- [Ruby String Interpolation](https://ruby-doc.org/core-2.7.2/doc/syntax/literals_rdoc.html#label-String+Interpolation)