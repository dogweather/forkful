---
title:    "Gleam: Verwendung von regulären Ausdrücken"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein mächtiges Werkzeug in der Welt der Programmierung. Sie ermöglichen es Entwicklern, komplexe Muster in Texten zu suchen und zu manipulieren. Wenn du häufig mit Texten arbeitest und deinen Workflow verbessern möchtest, solltest du unbedingt lernen, wie man reguläre Ausdrücke benutzt.

## So geht's

Mit Gleam kannst du reguläre Ausdrücke für verschiedene Aufgaben verwenden. Hier sind einige Beispiele:

### Email-Adressen überprüfen

Um zu überprüfen, ob eine Zeichenkette eine gültige E-Mail-Adresse ist, kannst du folgenden regulären Ausdruck verwenden:

```Gleam
let email_regex = Regex.compile("^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$")
let result = Regex.test(email_regex, "example@example.com")

io.print(result) // Output: true
```

### Wörter zählen

Du kannst auch reguläre Ausdrücke verwenden, um die Anzahl der Wörter in einem Text zu zählen:

```Gleam
let word_regex = Regex.compile("\\w+")
let sample_text = "Dies ist ein Beispieltext"
let result = Regex.count_matches(word_regex, sample_text)

io.print(result) // Output: 5
```

Es gibt noch viele weitere Anwendungsfälle für reguläre Ausdrücke in Gleam, also lass deiner Kreativität freien Lauf!

## Tiefere Einblicke

Reguläre Ausdrücke können komplex und verwirrend erscheinen, vor allem wenn man mit ihnen noch nicht vertraut ist. Aber keine Sorge, mit ein wenig Übung kannst du sie meistern! Hier sind einige wichtige Konzepte zu beachten:

- Zeichenklassen: Mit eckigen Klammern kannst du angeben, welche Zeichen in einer bestimmten Position erlaubt sind. Zum Beispiel kann `[aeiou]` auf einen beliebigen Vokal passen.
- Quantifizierer: Diese Symbole geben an, wie oft ein bestimmtes Zeichen oder eine Zeichengruppe vorkommen muss. Zum Beispiel steht `+` für "ein oder mehrere" und `?` für "null oder eins".
- Angrenzende Ziffern: Mit einer Gruppierung von Ziffern in Klammern kannst du bestimmte Teile des Ausdrucks auswählen und extrahieren. Diese Informationen können in deinem Code weiter verwendet werden.

Für weitere Details und Beispiele empfehlen wir dir, die offizielle Gleam-Dokumentation zu regulären Ausdrücken zu konsultieren.

## Siehe auch

- [Offizielle Gleam-Dokumentation zu regulären Ausdrücken](https://gleam.run/documentation/guides/regular_expressions.html)
- [Eine interaktive Einführung in reguläre Ausdrücke](https://www.regexone.com/)
- [Weitere Gleam-Blog-Posts (auf Deutsch)](https://gleam.run/news/)