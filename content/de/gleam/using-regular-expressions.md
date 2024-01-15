---
title:                "Die Verwendung von regulären Ausdrücken"
html_title:           "Gleam: Die Verwendung von regulären Ausdrücken"
simple_title:         "Die Verwendung von regulären Ausdrücken"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Lassen Sie uns ehrlich sein, reguläre Ausdrücke können manchmal verwirrend sein. Aber in der Welt des Programmierens, können sie auch verdammt nützlich sein! Wenn Sie Ihre Textmanipulation auf die nächste Stufe bringen möchten, ist die Beherrschung der regulären Ausdrücke ein Muss.

## Wie Sie reguläre Ausdrücke in Gleam verwenden

In Gleam bietet uns das `gleam/regex` Paket die Möglichkeit zur Verwendung von regulären Ausdrücken. Hier ist ein Beispiel dafür, wie man eine E-Mail-Adresse mit regulären Ausdrücken validieren kann:

```Gleam
let valid_email = regex?("([\\w-\\.]+)@([\\w]+\\.{1,5}([\\w]+))", "john.doe@example.com")
```

In diesem Beispiel wird die E-Mail-Adresse "john.doe@example.com" gegen einen regulären Ausdruck geprüft, der auf die Standardformatierung von E-Mail-Adressen abzielt. Das Ergebnis des Ausdrucks wird in der Variablen `valid_email` gespeichert und kann genutzt werden, um zu überprüfen, ob die E-Mail-Adresse valide ist.

Um den Inhalt einer Zeichenkette anhand eines regulären Ausdrucks zu ersetzen, können wir die Funktion `regex_replace` verwenden. Sehen wir uns ein Beispiel an:

```Gleam
let replaced_text = regex_replace("Gleam", "Hello Gleam!", "Hola")
```

In diesem Fall wird der Text "Hello Gleam!" anhand eines regulären Ausdrucks nach dem gesuchten Begriff "Gleam" durch "Hola" ersetzt. Das Ergebnis wird in der Variablen `replaced_text` gespeichert und kann ausgegeben werden.

## Tieferes Eintauchen in reguläre Ausdrücke

Reguläre Ausdrücke können viel mehr als nur simple Textüberprüfung und -ersetzungen. Sie können auch verwendet werden, um Texte zu teilen, Gruppen zu bilden und vieles mehr. Es gibt verschiedene Symbole und Ausdrücke, die in regulären Ausdrücken verwendet werden können, um bestimmte Patterns zu suchen und zu manipulieren. Wenn Sie tiefer in die Welt der regulären Ausdrücke eintauchen möchten, empfehlen wir Ihnen, die offizielle Gleam-Dokumentation zum Paket `gleam/regex` zu lesen.

## Siehe auch

- [Gleam-Dokumentation zu regulären Ausdrücken](https://gleam.run/modules/regex)
- [Interactive tutorial zu regulären Ausdrücken](https://regexone.com/) (in englischer Sprache)
- [RegExr](https://regexr.com/) - ein nützliches Tool zum Testen und Üben von regulären Ausdrücken (in englischer Sprache)