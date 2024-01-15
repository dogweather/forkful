---
title:                "Ein neues Projekt beginnen"
html_title:           "Gleam: Ein neues Projekt beginnen"
simple_title:         "Ein neues Projekt beginnen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum
Das Erstellen eines neuen Projekts kann eine aufregende Herausforderung sein und bietet die Möglichkeit, kreative Ideen umzusetzen und neue Fähigkeiten zu erlernen. Gleam, eine von Erlang inspirierte funktionale Sprache, bietet eine schnelle und sichere Umgebung für die Entwicklung von Code. Diese Kombination macht es zu einer großartigen Wahl, um neue Projekte zu starten.

## Wie es geht
Um ein neues Projekt mit Gleam zu starten, folge einfach diesen Schritten:

1. Installiere Gleam auf deinem Computer.
2. Öffne ein Terminalfenster und navigiere zum gewünschten Speicherort für dein Projekt.
3. Gib den Befehl `gleam new project_name` ein und drücke die Eingabetaste.
4. Gleam erstellt automatisch einen Ordner mit dem Namen deines Projekts und initialisiert es mit einer grundlegenden Struktur.
5. Navigiere in den neu erstellten Projektordner und öffne die Datei `src/main.gleam` in deinem bevorzugten Texteditor.
6. Beginne mit der Definition deiner Module und Funktionen und schreibe deinen Code innerhalb der `main`-Funktion.
7. Führe deinen Code aus, indem du in das Terminal-Fenster wechselst und den Befehl `gleam run` eingibst. Du solltest die Ausgabe deines Programms sehen.

Hier ist ein Beispiel für einfachen Code, der die Quintessenz von Gleam demonstriert - Typsicherheit und Pattern Matching:

```Gleam
foo() {
  let x = 42
  x // Funktion gibt 42 zurück
}

case foo() {
  x -> "Result is ${x}" // Pattern-Matching erkennt, dass x 42 ist
}
```

Kurz gesagt, Gleam bietet dir eine schnelle und sichere Möglichkeit, Code zu schreiben. Also zögere nicht und starte dein neues Projekt noch heute!

## Tief in die Materie eintauchen
Bevor du mit der Entwicklung deines Projekts beginnst, kannst du dir etwas Zeit nehmen, um tiefer in Gleam einzutauchen und mehr über seine Möglichkeiten zu erfahren. Hier sind einige Ressourcen, die dir dabei helfen können: 

- Die offizielle Gleam-Website: [gleam.run](https://gleam.run/)
- Die Dokumentation: [docs.gleam.run/](https://docs.gleam.run/)
- Der offizielle Gleam-Chat: [gleam-lang/gleam on Gitter](https://gitter.im/gleam-lang/gleam)

## Siehe auch
- [Gleam-Repository auf GitHub](https://github.com/gleam-lang/gleam)
- [Erlang-Website](https://www.erlang.org/)
- [Funktionale Programmierung für Anfänger: Was ist Gleam?](https://www.freecodecamp.org/news/functional-programming-for-beginners-what-is-gleam-63eb1b33fe5/)