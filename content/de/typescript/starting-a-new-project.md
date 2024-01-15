---
title:                "Ein neues Projekt beginnen"
html_title:           "TypeScript: Ein neues Projekt beginnen"
simple_title:         "Ein neues Projekt beginnen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum

Wenn du ein neues Projekt startest, willst du wahrscheinlich eine neue Herausforderung angehen oder vielleicht sogar deine Fähigkeiten als Entwickler verbessern. Es gibt jedoch auch praktische Gründe, ein neues TypeScript-Projekt zu starten, wie zum Beispiel die Verwendung neuer Technologien oder die Entwicklung einer maßgeschneiderten Lösung für ein bestimmtes Problem.

## Wie geht das?

Um ein neues TypeScript-Projekt zu starten, musst du zuerst sicherstellen, dass du Node.js installiert hast. Dann kannst du das TypeScript-Paket mit dem folgenden Befehl installieren:

```TypeScript
npm install -g typescript
```

Als nächstes erstellst du ein neues Verzeichnis für dein Projekt und initialisierst es als Node.js-Projekt:

```TypeScript
mkdir mein-projekt
cd mein-projekt
npm init -y
```

Packen wir nun den TypeScript-Compiler mit dem folgenden Befehl in unser Projekt:

```TypeScript
npm install --save-dev typescript
```

Jetzt kannst du eine TypeScript-Datei erstellen, zum Beispiel "app.ts", und deine Programmierkenntnisse in Aktion setzen:

```TypeScript
const name: string = "Max";
console.log("Hallo " + name);
```

Um deine TypeScript-Datei zu kompilieren, kannst du den folgenden Befehl verwenden:

```TypeScript
tsc app.ts
```

Dies wird eine neue JavaScript-Datei mit dem Namen "app.js" erstellen, die du dann ausführen kannst:

```TypeScript
node app.js
```

Wenn alles gut geht, sollte die Ausgabe "Hallo Max" sein.

## Tiefer gehen

Wenn du tiefer in das Erstellen von TypeScript-Projekten eintauchen möchtest, gibt es einige zusätzliche Dinge, die du berücksichtigen kannst. Zum Beispiel kannst du die Konfigurationsoptionen in der "tsconfig.json"-Datei verwenden, um den Compiler an deine Bedürfnisse anzupassen. Du kannst auch externe Bibliotheken einbinden, um mehr Funktionalität zu erhalten. Außerdem ist es ratsam, sich mit den Best Practices für das Schreiben von TypeScript-Code vertraut zu machen.

## Siehe auch

- ["TypeScript in 5 Minuten" von Microsoft](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
- ["Neue Projekte mit TypeScript starten" von stackchief](https://stackchief.com/blog/Creating%20new%20projects%20in%20TypeScript)
- ["TypeScript-Handbuch" von Microsoft](https://www.typescriptlang.org/docs/handbook/)