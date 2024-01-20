---
title:                "Ein neues Projekt starten"
html_title:           "C#: Ein neues Projekt starten"
simple_title:         "Ein neues Projekt starten"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein neues Projekt zu starten ist, wenn Sie von Grund auf mit der Entwicklung einer neuen Software beginnen. Programmierer tun dies, um eine spezifische Problemstellung zu lösen oder um ein Produkt zu schaffen, das ihnen vorschwebt.

## So geht's:

Die Aufsetzung eines Projekts in JavaScript kann durch verschiedene Tools wie z.B. `npm` (Node Package Manager) erleichtert werden.

```Javascript
// Installation von npm (nur einmal nötig)
npm install npm@latest -g

// Erstellen Sie ein neues Verzeichnis und navigieren Sie dorthin
mkdir MeinProjekt && cd $_

// Initialisieren Sie ein neues npm Projekt 
npm init -y

// Eine Datei erstellen
touch index.js
```

Dadurch wird eine `package.json` Datei in Ihrem Projektverzeichnis generiert, welche Informationen wie den Namen und die Version des Projekts beinhaltet.

```Javascript
{
  "name": "MeinProjekt",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  "keywords": [],
  "author": "",
  "license": "ISC"
}
```

## Tiefer Einblick:

Das Konzept des Startens neuer Projekte ist so alt wie die Programmierung selbst, jedoch haben Tools wie `npm` diesen Prozess erheblich vereinfacht und standardisiert. Alternativen zu `npm` sind `yarn` oder `pnpm`. Diese bieten ähnliche Funktionalitäten, aber unterscheiden sich in kleinen, spezifischen Aspekten. Die Wahl hängt also von Ihren persönlichen Präferenzen ab.

Der Start eines neuen Projekts beinhaltet oft mehr als nur das Erstellen einer `package.json` Datei. Zu weiteren wichtigen Schritten können z.B. das Konfigurieren eines Linters (zur Überprüfung des Code-Stils) oder das Einrichten eines Test-Frameworks gehören.

## Siehe auch:

- [NPM Dokumentation](https://docs.npmjs.com/)
- [Yarn Dokumentation](https://classic.yarnpkg.com/lang/en/)
- [pnpm Dokumentation](https://pnpm.io/)