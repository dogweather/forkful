---
title:    "Gleam: Ein neues Projekt beginnen."
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Warum
Das Erstellen eines neuen Projekts in Gleam kann eine aufregende Gelegenheit sein, um Ihre Fähigkeiten in der funktionalen Programmierung zu verbessern und neue Herausforderungen anzunehmen. Mit Gleam können Sie elegante und robuste Code schreiben, der leicht zu lesen und zu warten ist.

## Wie geht's
Um ein neues Projekt in Gleam zu starten, führen Sie folgende Schritte aus:

1. Installieren Sie zunächst die neueste Version von Gleam auf Ihrem Computer.
2. Öffnen Sie Ihr Terminal und navigieren Sie in das gewünschte Verzeichnis für Ihr Projekt.
3. Verwenden Sie den Befehl `gleam new project_name` um ein neues Projekt mit dem angegebenen Namen zu erstellen.
4. Navigieren Sie in das neu erstellte Projektverzeichnis und öffnen Sie es in Ihrem bevorzugten Code-Editor.
5. Beginnen Sie mit der Programmierung in Gleam und verwenden Sie die vielfältigen Funktionen, die die Sprache bietet, um Ihr Projekt zu erstellen.

Um Ihnen den Einstieg zu erleichtern, hier ein einfaches Beispiel für eine Gleam-Funktion, die zwei Zahlen multipliziert:

```Gleam
fn multiplizieren(a, b) {
  let ergebnis = a * b
  ergebnis
}
```

Das könnte die Ausgabe sein, wenn Sie die Funktion mit den Argumenten `2` und `3` aufrufen:

```Gleam
multiplizieren(2, 3)
=> 6
```

## Tiefere Einblicke
Bevor Sie Ihre Programmierung starten, sollten Sie sich gut mit den Grundlagen von Gleam vertraut machen. Lesen Sie die offizielle Dokumentation und probieren Sie verschiedene Code-Beispiele aus, um ein besseres Verständnis von der Sprache zu bekommen. Denken Sie auch daran, dass Gleam eine statisch typisierte Sprache ist, was bedeutet, dass Sie Variablen bestimmte Typen zuweisen müssen. Nehmen Sie sich Zeit, um sich mit den Typen vertraut zu machen und wie Sie sie in Ihrem Code verwenden können.

Wenn Sie ein größeres Projekt starten möchten, sollten Sie sich auch mit dem Konzept der Module und des Imports in Gleam vertraut machen. Dies wird Ihnen helfen, Ihren Code besser zu organisieren und die Wiederverwendung von Code-Teilen zu fördern.

## Siehe auch
- [Offizielle Gleam-Dokumentation](https://gleam.run)
- [Beispiele für Gleam-Code](https://github.com/gleam-lang/gleam/tree/master/examples)
- [Gleam-Kurs auf Udemy (auf Deutsch)](https://www.udemy.com/course/gleam-the-language-for-scalable-erlang-applications/)