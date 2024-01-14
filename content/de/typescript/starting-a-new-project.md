---
title:                "TypeScript: Eine neue Projektarbeit beginnen"
programming_language: "TypeScript"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, ein neues Programmierprojekt zu starten. Vielleicht möchtest du deine Fähigkeiten verbessern, ein bestimmtes Problem lösen oder ein persönliches Ziel erreichen. Wie auch immer, TypeScript ist eine großartige Wahl für dein nächstes Projekt, da es eine typisierte Sprache ist, die auf JavaScript aufbaut und die Entwicklung erleichtert.

## Anleitung

Um TypeScript in deinem Projekt zu verwenden, musst du zuerst die TypeScript-Compiler-Tools installieren. Dies kannst du im Terminal mit dem Befehl ```npm install -g typescript``` machen. Jetzt kannst du deine TypeScript-Dateien mit dem Befehl ```tsc``` kompilieren und die erzeugte JavaScript-Datei ausführen.

In TypeScript kannst du Variablen mit dem Schlüsselwort ```let``` definieren und Typen mit Doppelpunkten angeben, zum Beispiel ```let name: string = "Max"```. Hier gibt es noch weitere Datentypen wie zum Beispiel ```number```, ```boolean``` und ```any```, um nur einige zu nennen.

Eine praktische Funktion in TypeScript ist die Möglichkeit, benutzerdefinierte Typen zu erstellen. Dies kannst du mit dem Schlüsselwort ```interface``` tun, um eine Schablone für ein Objekt zu definieren, zum Beispiel:

```
interface Person {
  name: string;
  age: number;
  hasPets: boolean;
}
```

Jetzt kannst du eine Person mit diesen Eigenschaften erstellen und die Typisierung sorgt dafür, dass diese Eigenschaften beim Programmieren nicht versehentlich geändert werden.

## Tiefere Einblicke

Bevor du dein neues Projekt startest, solltest du dir Gedanken darüber machen, wie du es strukturieren möchtest. TypeScript ist perfekt für die Verwendung mit dem Model-View-Controller (MVC) Entwurfsmuster, da es die Kapselung von Daten und Funktionen erleichtert.

Eine weitere wichtige Überlegung ist die Verwendung von Typen. Während es verlockend sein kann, überall den Typ ```any``` zu verwenden, solltest du versuchen, so viele konkrete Typen wie möglich zu verwenden, um sicherzustellen, dass dein Code gut gewartet werden kann.

Schließlich, um das Beste aus TypeScript herauszuholen, solltest du dir die vielen verfügbaren Bibliotheken und Frameworks ansehen, die speziell für TypeScript entwickelt wurden. Diese können dir helfen, effizienter und strukturierter zu programmieren.

## Siehe auch

- [Offizielle TypeScript-Dokumentation](https://www.typescriptlang.org/docs/home.html)
- [Liste von TypeScript-Bibliotheken und Frameworks](https://awesome-typescript.com/)