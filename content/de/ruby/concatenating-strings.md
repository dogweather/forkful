---
title:                "Ruby: Verknüpfen von Zeichenfolgen"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Ah, und noch ein weiterer Tag, an dem du dich mit dem Kampf der Zeichenketten auseinandersetzen musst. Aber warum eigentlich? Warum sollte jemand Zeit damit verbringen, Strings zu verbinden? Nun, es gibt viele Gründe dafür, aber der Hauptgrund ist die Manipulation von Daten. Wenn du mit Strings arbeitest, ist es oft notwendig, sie zu kombinieren, um eine bestimmte Formatierung zu erreichen oder um sie in bestimmten Funktionen oder Algorithmen zu verwenden. Also wenn du dich schon einmal gefragt hast, warum du ständig auf Zeichenketten-Konkatenation stößt, hier ist deine Antwort.

## Wie geht man vor?

Um Zeichenketten in Ruby zu verbinden, gibt es mehrere Möglichkeiten, aber die einfachste ist die Verwendung des `+` Operators. Stell dir zum Beispiel vor, du hast zwei Variablen, `name` und `greeting`, und du möchtest den Namen mit einer Grußnachricht verbinden. Hier ist ein Beispielcode mit Ausgabe:

```Ruby
name = "Anna"
greeting = "Hallo"

puts greeting + " " + name
```

Die Ausgabe hier wäre:

```
Hallo Anna
```

Eine andere Möglichkeit, Zeichenketten zu verbinden, ist die Verwendung der `<<` Methode. Schau dir das folgende Beispiel an:

```Ruby
fruit1 = "Apfel"
fruit2 = "Birne"

fruit1 << fruit2
```

In diesem Fall wird `fruit2` an `fruit1` angehängt, so dass `fruit1` jetzt "ApfelBirne" lautet.

## Tief tauchen

Jetzt wo wir wissen, wie man Zeichenketten verbindet, schauen wir uns etwas tiefer in die Materie an. Du hast vielleicht bemerkt, dass wir bei Verwendung des `+` Operators Leerzeichen hinzufügen mussten, um die Namen und Grüße getrennt zu halten. Aber was wäre, wenn wir ein Leerzeichen an eine Variable anhängen möchten, ohne es explizit in den Code zu schreiben? Hier kommt die `concat` Methode ins Spiel. Schau dir dieses Beispiel an:

```Ruby
name = "Maria"
greeting = "Hallo"

greeting.concat(" ", name)
```

Die Ausgabe wäre die gleiche wie beim vorherigen Beispiel, aber dieses Mal haben wir das Leerzeichen durch die `concat` Methode hinzugefügt. Diese Methode fügt das Argument am Ende des ursprünglichen Strings an, ohne eine neue Kopie des Strings zu erstellen.

## Siehe auch

- [Ruby String Klasse Dokumentation](https://ruby-doc.org/core-2.7.2/String.html)
- [Concatenating Strings in Ruby (Englisch)](https://www.rubyguides.com/2019/06/ruby-string-concat)
- [Ruby Methoden: Unterschied zwischen `+` und `<<` (Englisch)](https://thoughtbot.com/blog/ruby-methods-add-vs-eql-less-than-less-than)