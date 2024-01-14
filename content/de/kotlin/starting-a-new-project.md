---
title:                "Kotlin: Ein neues Projekt beginnen"
programming_language: "Kotlin"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum
Wenn Sie in die Welt des Programmierens eintauchen möchten, kann die Erstellung eines neuen Projekts eine aufregende Möglichkeit sein, Ihre Fähigkeiten zu verbessern und neue Dinge zu lernen.

## Wie geht das
Um ein neues Projekt in Kotlin zu starten, müssen Sie zuerst die Kotlin-Entwicklungsumgebung (IDE) installieren. Wir empfehlen IntelliJ IDEA, da es eine hervorragende Unterstützung für Kotlin bietet. Sobald Sie die IDE geöffnet haben, können Sie ein neues Projekt erstellen, indem Sie auf `File -> New Project` klicken und Kotlin als Programmiersprache auswählen. Vergessen Sie nicht, Ihrem Projekt einen Namen zu geben und den Speicherort auszuwählen. 

Einmal erstellt, können Sie mit der Codierung beginnen. Hier sind einige Beispiele für Kotlin-Code, die Ihnen den Einstieg erleichtern können:

```Kotlin
// Einfache Ausgabe
fun main() {
    println("Hallo Welt!")
}
```

```Kotlin
// Variablen und Datentypen
fun main() {
    var name: String = "Anna"
    var alter: Int = 25
    var groesse: Double = 1.65

    println("$name ist $alter Jahre alt und ist $groesse Meter groß.")
}
```

```Kotlin
// Bedingungen und Schleifen
fun main() {
    var zahl: Int = 5

    // If-Else-Anweisung
    if (zahl % 2 == 0) {
        println("$zahl ist eine gerade Zahl.")
    } else {
        println("$zahl ist eine ungerade Zahl.")
    }

    // For-Schleife
    for (i in 1..10) {
        println("Dies ist Schleifeniteration Nr. $i.")
    }
}
```

## Tiefentauchgang
Bevor Sie mit der Codierung beginnen, ist es wichtig, eine klare Vorstellung von Ihrem Projekt zu haben. Überlegen Sie, welche Funktionen es haben soll, welche Probleme es lösen soll und wer Ihre Zielgruppe ist. Nehmen Sie sich Zeit für das Design und die Planung, um später frustrierende Änderungen zu vermeiden. 

Auch die Wahl der richtigen Tools und Bibliotheken für Ihr Projekt ist entscheidend. Eine gute Möglichkeit, die passenden Ressourcen zu finden, ist die Suche in Online-Communities oder das Lesen von Blogs und Tutorials. Vergessen Sie nicht, sauberen und gut strukturierten Code zu schreiben, um die Wartbarkeit und Erweiterbarkeit Ihres Projekts sicherzustellen.

## Siehe auch
- [Offizielle Kotlin Webseite](https://kotlinlang.org/de/)
- [IntelliJ IDEA](https://www.jetbrains.com/de-de/idea/)
- [Kotlin für Anfänger: Eine Einführung und Beispiele](https://www.raywenderlich.com/4504993-kotlin-for-android-beginners-introduction-and-examples)