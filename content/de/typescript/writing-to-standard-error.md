---
title:                "Schreiben auf Standardfehler"
html_title:           "TypeScript: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben an den Standardfehler ist eine Methode, die von Programmierern verwendet wird, um Fehler und Warnungen in der Konsole anzuzeigen. Dies ist eine gute Möglichkeit, um den Entwickler auf Probleme aufmerksam zu machen und das Debugging zu erleichtern.

## So geht's:
Um an den Standardfehler zu schreiben, verwenden wir die Funktion "console.error()". Hier ist ein Beispielcode in TypeScript:

```TypeScript
console.error("Ein Fehler ist aufgetreten!");
```
Die Ausgabe dieses Codes wäre:
`Ein Fehler ist aufgetreten!`

Um eine Variable als Fehlermeldung auszugeben, nutzen wir die Zeichenkette-Variante der Funktion:

```TypeScript
let num: number = 42;
console.error("Die Zahl " + num + " ist falsch!");
```
Die Ausgabe wäre dann:
`Die Zahl 42 ist falsch!`

## Tiefere Einblicke:
Das Schreiben an den Standardfehler ist eine häufig verwendete Technik, um auf Probleme hinzuweisen. Es hat seinen Ursprung in der UNIX-Welt, in der Fehlermeldungen häufig an die Konsole geleitet wurden. Eine Alternative zu dieser Methode ist das Schreiben an den Standardausgang (console.log()), aber dies wird meist für normale Ausgaben und nicht für Fehler verwendet.

Die Implementierung des Schreibens an den Standardfehler ist sehr einfach und kann in jedem zugrunde liegenden Betriebssystem oder Laufzeitumgebung anders sein.

## Siehe auch:
- https://developer.mozilla.org/de/docs/Web/API/console/error
- https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#-type-checked-calls-to-console-error-warn-and-info
- https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html#-new-never-type