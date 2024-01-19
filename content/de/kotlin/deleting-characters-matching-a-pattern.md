---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

---

# Entfernen Sie Zeichen in Kotlin, die einem Muster entsprechen

## Was & Warum?

Das Entfernen von Zeichen, die einem Muster entsprechen, ist ein Prozess, bei dem spezifische Zeichen aus einem Textstring entfernt werden. Programmierer tun dies, um unerwünschte oder überflüssige Daten aus ihren Codezeilen zu entfernen und die Gesamteffizienz ihrer Programme zu erhöhen.

## So geht's:

Verwenden Sie die replace-Funktion in Kotlin, um alle Instanzen eines bestimmten Zeichens oder Musters in einem String zu entfernen. 

 ```Kotlin
val text = "Hallo, Welt!"
val entfernterText = text.replace(",", "")
println(entfernterText)
 ```

Die Ausgabe dieses Codes wäre:

 `Hallo Welt!`

## Tiefere Tauchgänge:

Das Entfernen von Zeichen, die einem Muster entsprechen, stammt aus den Anfängen der Computerprogrammierung, als Entwickler Wege finden mussten, unerwünschte Daten aus ihren Codezeilen zu entfernen. Eine Alternative ist die Verwendung von regulären Ausdrücken, die es erlauben, auch komplexe Muster von Zeichen zu identifizieren und zu entfernen. 

Bei minimalistischer Implementierung ist zu beachten, dass die Funktionen `replace` und `replaceFirst` in Kotlin das ursprüngliche String-Objekt nicht ändern - sie geben eine neue Zeichenkette zurück, in der das Muster ersetzt wurde.

## Siehe auch:

Sie können weitere Informationen zu diesem Thema unter den folgenden Quellen finden:

- JetBrains offizielle Kotlin-Dokumentation : <https://kotlinlang.org/docs/stdlib.html>
- Ein tiefer Einblick in String Manipulationen in Kotlin: <https://blog.kotlin-academy.com/kotlin-programmer-dictionary-string-vs-string-companion-object-689e5c56aed5>
  
---