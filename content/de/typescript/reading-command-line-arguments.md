---
title:    "TypeScript: Lesen von Befehlszeilenargumenten"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist eine wichtige Fähigkeit für jeden, der in TypeScript programmieren möchte. Es ermöglicht es uns, unsere Programme interaktiver und flexibler zu gestalten, indem wir es Benutzern ermöglichen, Eingaben direkt in der Kommandozeile zu machen. Hier erfährst du, wie du in TypeScript Befehlszeilenargumente lesen kannst.

## Wie das geht

Um Befehlszeilenargumente in TypeScript zu lesen, verwenden wir das `process` Modul. Wir können es in unser Programm mit dem folgenden Code importieren:

```TypeScript
import * as process from 'process';
```

Um auf die Befehlszeilenargumente zuzugreifen, rufen wir einfach `process.argv` auf. Diese Eigenschaft gibt ein Array von Strings zurück, wobei der erste Eintrag den Pfad zur Node.js Installation und der zweite Eintrag den Pfad zur ausgeführten Datei enthält. Die folgenden Einträge enthalten dann die übergebenen Befehlszeilenargumente.

Beispiel:

```TypeScript
// node main.ts hello world
console.log(process.argv);
// Ausgabe: ["/usr/local/bin/node", "main.ts", "hello", "world"]
```

Wir können auch überprüfen, ob bestimmte Befehlszeilenargumente vorhanden sind, indem wir `process.argv` durchsuchen oder die `includes()` -Methode verwenden.

Beispiel:

```TypeScript
// node main.ts --debug
console.log(process.argv.includes('--debug'));
// Ausgabe: true
```

## Tiefer Einblick

Neben dem Zugriff auf die übergebenen Befehlszeilenargumente können wir auch mithilfe des `yargs` Moduls, das eine benutzerfreundlichere API bietet, komplexe Befehlszeilenoptionen verarbeiten.

Wir können das `yargs` Modul mit dem folgenden Code installieren und importieren:

```TypeScript
npm install yargs
import * as yargs from 'yargs';
```

Dann können wir `yargs` verwenden, um unsere Befehlszeilenargumente mit verschiedenen Optionen zu verarbeiten und beispielsweise Standardwerte festzulegen.

Beispiel:

```TypeScript
const argv = yargs
    .option('name', {
        alias: 'n',
        default: 'World',
        describe: 'The name to greet'
    })
    .option('greeting', {
        alias: 'g',
        default: 'Hello',
        describe: 'The greeting to use'
    })
    .argv;

console.log(`${argv.greeting}, ${argv.name}!`);
// node main.ts --greeting="Bonjour" --name="Monde"
// Ausgabe: Bonjour, Monde!
```

## Siehe auch

- [Dokumentation zum `process` Modul in Node.js](https://nodejs.org/api/process.html)
- [Dokumentation zum `yargs` Modul](https://www.npmjs.com/package/yargs)