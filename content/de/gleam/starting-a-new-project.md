---
title:                "Gleam: Ein neues Projekt beginnen."
simple_title:         "Ein neues Projekt beginnen."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Warum

Wenn du ein Programmierer oder eine Programmiererin bist, hast du sicherlich schon von Gleam gehört. Diese funktionale Programmiersprache ist auf dem Vormarsch und hat eine wachsende Community von Entwicklern und Entwicklerinnen. Aber warum sollte man sich überhaupt die Mühe machen, eine neue Programmiersprache zu lernen und ein Gleam-Projekt zu starten?

Gleam bietet einige einzigartige Vorteile, die es zu einer lohnenden Wahl für dein nächstes Projekt machen. Mit seiner statischen Typisierung und seiner starken Unterstützung für Nebenläufigkeit und Parallelität ist Gleam ideal für die Entwicklung robuster und skalierbarer Anwendungen. Auch die einfache Syntax und das benutzerfreundliche Tooling machen es für Entwickler und Entwicklerinnen jeder Erfahrungsstufe zugänglich.

# Wie man anfängt

Um ein Gleam-Projekt zu starten, musst du zunächst die Gleam-Tools auf deinem Computer installieren. Dies ist mit einem einzigen Befehl möglich, der für deine Betriebssystem spezifisch ist. Sobald dies erledigt ist, kannst du ein neues Projekt mit dem Befehl `gleam new` erstellen.

Nun kannst du deine ersten Gleam-Zeilen schreiben! Hier ist ein Beispiel, wie du eine Funktion schreibst, die die Fakultät einer Zahl berechnet:

```Gleam
pub fn factorial(n) {
    if n < 2 {
        1
    } else {
        n * factorial(n - 1)
    }
}

pub fn main() {
    let result = factorial(5)
    |> io.format("The factorial of 5 is {}", _)
    io.println(result)
}
```

Das Ausführen dieses Codes würde 120 als Ergebnis ausgeben. Wie du sehen kannst, ist Gleam einfach zu erlernen und zu benutzen. Aber das war nur ein kleiner Vorgeschmack, lass uns nun tiefer in die Welt von Gleam eintauchen.

# Tiefergehende Informationen

Wenn du wirklich in die Feinheiten von Gleam eintauchen möchtest, gibt es einige Dinge, die du beachten solltest. Gleam ist eine statisch typisierte Sprache, was bedeutet, dass du Variablen einen bestimmten Datentyp zuweisen musst. Gleam unterstützt auch eine starke Typinferenz, was bedeutet, dass der Compiler in der Lage ist, viele Typen automatisch zu erkennen, ohne dass du sie explizit angeben musst.

Eine weitere wichtige Funktion von Gleam ist seine Unterstützung für Nebenläufigkeit und Parallelität. Nebenläufigkeit bezieht sich auf die Fähigkeit, mehrere Aufgaben gleichzeitig auszuführen, während Parallelität bedeutet, dass diese Aufgaben tatsächlich auf verschiedenen Prozessorkernen gleichzeitig ausgeführt werden. Gleam macht es einfach, diese Konzepte in deinem Code zu implementieren, was zu schnelleren und effizienteren Anwendungen führen kann.

# Siehe auch

- Offizielle Gleam-Website: https://gleam.run/
- Gleam-Dokumentation: https://gleam.run/documentation/
- Gleam-Tutorial: https://dev.to/kofi/learn-gleam-programming-language-from-scratch-5h0d