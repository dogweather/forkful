---
title:                "Gleam: Suchen und Ersetzen von Text"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum?

Für Programmierer ist das Suchen und Ersetzen von Text eine alltägliche Aufgabe. Es ist ein wichtiger Teil des Programmierprozesses und kann dabei helfen, effizienter und schneller zu arbeiten. In diesem Blogpost werden wir uns genauer mit der Such- und Ersetzungsfunktion in Gleam befassen und zeigen, wie sie eingesetzt werden kann, um Zeit und Mühe zu sparen.

## Wie

Die Such- und Ersetzungsfunktion in Gleam ist sehr einfach zu bedienen. Man kann sie auf verschiedene Arten aufrufen: entweder über die eingebaute ```.replace()``` Funktion oder über das Modul ```String.replace()```.

Ein Beispiel für die Verwendung von ```.replace()``` könnte wie folgt aussehen:

```Gleam
let original_text = "Dies ist ein Beispieltext."
let replaced_text = original_text.replace("Beispieltext", "neuer Text")
```

Das Ergebnis wird in der Variablen ```replaced_text``` gespeichert und lautet "Dies ist ein neuer Text." Wir können auch reguläre Ausdrücke verwenden, um gezielt Text zu suchen und zu ersetzen. Ein Beispiel dafür wäre:

```Gleam
let original_text = "Dies ist ein Beispieltext."
let replaced_text = original_text.replace([regex"\s+", "g"], "")
```

In diesem Fall würden alle Leerzeichen im Text durch nichts ersetzt werden, was zu der Ausgabe "DiesisteinBeispieltext." führen würde.

Ein weiteres nützliches Beispiel für die Such- und Ersetzungsfunktion ist die Verwendung von Mustern und Backreferenzen. Mit diesen können wir gezielt nach bestimmten Textmustern suchen und sie ersetzen. Ein Beispiel dafür wäre:

```Gleam
let original_text = "Heute ist der 20. Juni."
let replaced_text = original_text.replace([regex"(\d+)\.", "g"], [pattern"$1. Tag", ""])
```

Das Ergebnis wäre "Heute ist der 20. Tag Juni."

## Deep Dive

Für fortgeschrittenere Anwendungen gibt es auch die Möglichkeit, benutzerdefinierte Such- und Ersetzungsfunktionen zu schreiben. Diese können dann in verschiedenen Szenarien eingesetzt werden, zum Beispiel beim Entwickeln von Textverarbeitungsprogrammen oder bei der Verarbeitung großer Datensätze.

Um eine benutzerdefinierte Funktion zu schreiben, können wir das Gleam-Modul ```String.Match``` verwenden. Dieses bietet Funktionen wie ```replace_first()``` und ```replace_all()```, die für komplexe Such- und Ersetzungsvorgänge nützlich sein können.

## Siehe auch

- Offizielle Gleam-Dokumentation: https://gleam.run/
- Eine Einführung in reguläre Ausdrücke: https://tutorialedge.net/golang/go-regex-tutorial/
- Dieser Blogpost auf GitHub: https://github.com/username/article.md