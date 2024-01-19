---
title:                "Ein neues Projekt starten"
html_title:           "C#: Ein neues Projekt starten"
simple_title:         "Ein neues Projekt starten"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Neue Projekte in TypeScript beginnen: ein praktischer Leitfaden

## Was & Warum?
Ein neues Projekt beginnen bedeutet, von Grund auf neu zu starten: neue Codebase, neue Dependencies und vielleicht eine neue Architektur. Programmierer tun dies, um innovative Lösungen zu entwickeln, zu lernen oder einzigartige Probleme zu lösen.

## Wie Zum:
Ein neues TypeScript-Projekt kann leicht mit dem Node-Paketmanager`npm` und TypeScript selbst gestartet werden. Hier ist wie:

Installieren Sie zuerst Node.js und npm auf Ihrem System, wenn dies noch nicht geschehen ist. Gehe zu [Node.js](https://nodejs.org/en/)seite.

```bash
# TypeScript installieren
$ npm install -g typescript

# Leeres Projektverzeichnis erstellen
$ mkdir MeinTypeScriptProjekt && cd MeinTypeScriptProjekt

# Eine neue TypeScript-Konfigurationsdatei erstellen
$ tsc --init

# Paket.json-Datei erstellen, um Ihre Abhängigkeiten zu verwalten
$ npm init -y
```

Ihr Projekt ist nun bereit für die Entwicklung. Sie können Ihre TypeScript-Dateien im Verzeichnis /src erstellen und die Transpiler mit `tsc` laufen lassen.

## Vertiefung
Das Beginnen neuer Projekte hat eine lange Geschichte in der Softwareentwicklung - es ist die Essenz des Erstellens. TypeScript selbst wurde von Microsoft entwickelt und ist eine typisierte Obermenge von JavaScript, die viele Probleme und Einschränkungen löst, die mit der Entwicklung großer Codebases in JavaScript verbunden sind.

Alternativen zu TypeScript sind Flow (von Facebook entwickelt) und PureScript. Während TypeScript bekannt für seine Strenge und tiefe Integration in die Entwicklungsumgebung ist, bietet Flow eine sanftere Typüberprüfung, und PureScript ist eine rein funktionalere Alternative.

Bei der Implementierung von TypeScript ist zu beachten, dass TypeScript bei der Transpilierung von TypeScript-Code zu JavaScript möglicherweise einige Type-Casting-Probleme übersehen kann. Es ist immer sicherer, notwendige Typüberprüfungen manuell durchzuführen.

## Siehe auch
Für weitere Informationen zu TypeScript und wie Sie mit neuen Projekten beginnen können, sehen Sie sich die folgenden Links an:

- [Offizielle TypeScript-Dokumentation](https://www.typescriptlang.org/docs/)
- [TypeScript Deep Dive](https://basarat.gitbook.io/typescript/)
- [TypeScript-Kurs auf Udemy](https://www.udemy.com/course/typescript-the-complete-developers-guide)