---
title:    "Gleam: Verwendung von regulären Ausdrücken"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein mächtiges Werkzeug, das dir helfen kann, Texte zu durchsuchen, zu filtern, und zu manipulieren. Sie erlauben es dir, komplexe Muster zu definieren und effizient auf große Mengen an Daten anzuwenden. Mit regulären Ausdrücken kannst du deine Programmierfähigkeiten auf ein neues Level bringen und deine Arbeitsabläufe enorm vereinfachen.

## Wie man reguläre Ausdrücke in Gleam verwendet

Die Grundlage für die Verwendung von regulären Ausdrücken in Gleam ist das Regex-Modul. Zuerst musst du dieses in deinem Programm importieren:

```
`` `Gleam
import Regex
`` ``
```

Um einen Text auf ein bestimmtes Muster zu überprüfen, kannst du die `Regex.matches` Funktion verwenden:

```
```Gleam
let text = "Hallo, mein Name ist Lisa."
let pattern = Regex.pattern("Name ist ([A-Za-z]+).")
let result = Regex.matches(pattern, text)

// Result: Ok([|"Name ist Lisa,"|])
```

In diesem Beispiel erstellen wir ein Regex-Muster, das nach einer Zeichenfolge sucht, die mit "Name ist" beginnt, gefolgt von einem Leerzeichen und mindestens einem Buchstaben. Dann überprüfen wir den Text und erhalten als Resultat eine Liste mit allen Übereinstimmungen.

Um weitere Details über eine Übereinstimmung zu erhalten, kannst du die `Regex.match` Funktion verwenden. Diese gibt ein Match-Objekt zurück, das verschiedene Methoden hat, um Details über die Übereinstimmung abzurufen:

```
```Gleam
let text = "Die Telefonnummer von Max ist 0123456789."
let pattern = Regex.pattern("Telefonnummer von ([A-Za-z]+) ist ([0-9]+).")
let result = Regex.match(pattern, text)

// Result: Ok(match {
// whole = "Telefonnummer von Max ist 0123456789.",
// captures = Ok(["Max", "0123456789"]),
// a = 9,
// b = 31
// });
```

Hier sehen wir, dass wir nicht nur das Übereinstimmungsmuster für den vollständigen Text erhalten, sondern auch die erfassten Werte für die beiden Platzhalter "Max" und "0123456789". Weiterhin gibt es Informationen über den Start- und Endindex der Übereinstimmung im ursprünglichen Text.

## Tiefergehende Informationen

Eine vollständige Übersicht über alle Funktionen im Regex-Modul findest du in der offiziellen Gleam-Dokumentation unter [https://gleam.run/modules/regex.html](https://gleam.run/modules/regex.html). Dort werden auch verschiedene Möglichkeiten aufgezeigt, wie du reguläre Ausdrücke in deinem Code effektiv einsetzen kannst.

Eine weitere hilfreiche Ressource ist die Seite [https://regex101.com/](https://regex101.com/), auf der du Regex-Muster interaktiv testen und Fehler beheben kannst.

## Siehe auch

- [https://gleam.run/modules/regex.html](https://gleam.run/modules/regex.html)
- [https://regex101.com/](https://regex101.com/)
- [https://www.regular-expressions.info/tutorial.html](https://www.regular-expressions.info/tutorial.html)