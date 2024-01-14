---
title:                "Kotlin: Die Verwendung von regulären Ausdrücken"
simple_title:         "Die Verwendung von regulären Ausdrücken"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung gibt es immer wieder Situationen, in denen man nach bestimmten Mustern in Texten suchen, ersetzen oder extrahieren muss. Hier kommen sogenannte reguläre Ausdrücke (englisch: regular expressions oder kurz: regex) ins Spiel. Mit ihrer Hilfe können komplexe Suchmuster definiert und effizient auf Texten angewendet werden. Reguläre Ausdrücke sind ein unverzichtbares Werkzeug für jeden, der regelmäßig mit Textverarbeitung in der Programmierung zu tun hat.

## Wie funktioniert es

Die Verwendung von regulären Ausdrücken in Kotlin ist sehr einfach und schnell zu erlernen. Zunächst muss die Klasse Regex importiert werden, um reguläre Ausdrücke zu definieren. Anschließend kann ein regulärer Ausdruck mithilfe von Anführungszeichen innerhalb eines String-Objekts erstellt werden. Hier ein einfaches Beispiel:

```Kotlin
val regex = Regex("hello")
println(regex.containsMatchIn("Hello World!")) // Ausgabe: true
println(regex.find("Hello World!")) // Ausgabe: MatchResult("hello")
```

Man kann auch angeben, ob der reguläre Ausdruck nur am Anfang oder am Ende des Textes gefunden werden soll, indem man "^" oder "$" entsprechend mit eingibt:

```Kotlin
val regex = Regex("^Hello")
println(regex.containsMatchIn("Hello World!")) // Ausgabe: true
println(regex.containsMatchIn("Bye Hello")) // Ausgabe: false
```

Auch die Verwendung von Quantoren wie "*" oder "+" ist möglich, um zu definieren, wie oft ein bestimmtes Zeichen oder Zeichenmuster vorkommen soll:

```Kotlin
val regex = Regex("c.*t")
println(regex.containsMatchIn("cat")) // Ausgabe: true
println(regex.containsMatchIn("coconut")) // Ausgabe: true
println(regex.containsMatchIn("cut")) // Ausgabe: true
println(regex.containsMatchIn("custard")) // Ausgabe: true
```

## Deep Dive

Reguläre Ausdrücke bieten jedoch noch viele weitere Möglichkeiten, die über die einfache Textsuche hinausgehen. Es können beispielsweise Teile eines Textes extrahiert oder ersetzt werden, und sogar komplexere Muster wie z.B. Telefonnummern oder E-Mail-Adressen können erkannt werden.

Eine Liste mit den verschiedenen Konstruktoren und Methoden der Klasse Regex sowie deren Funktionen und Verwendung ist auf der offiziellen Kotlin-Website zu finden. Außerdem gibt es zahlreiche Tutorials und Ressourcen online, die dabei helfen können, reguläre Ausdrücke in der Programmierung effektiv einzusetzen.

## Siehe auch

- [Offizielle Dokumentation zu regulären Ausdrücken in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Einführung in reguläre Ausdrücke in Kotlin](https://www.baeldung.com/kotlin-regex)
- [Reguläre Ausdrücke 101: Eine grundlegende Einführung](https://www.regular-expressions.info/tutorial.html)
- [RegExr: Kostenlose Online-RegEx-Tester und -Grafikgenerator](https://regexr.com/)