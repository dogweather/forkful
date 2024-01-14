---
title:                "Javascript: Lesen von Befehlszeilenargumenten"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Warum

Das Lesen von Befehlszeilenargumenten ist eine wichtige Fähigkeit in der Programmierung, die es uns ermöglicht, unser Programm auf verschiedene Wege auszuführen. Durch das Einlesen von Argumenten können wir unsere Anwendung flexibler gestalten und sie an die Bedürfnisse unserer Nutzer anpassen.

##Wie man Befehlszeilenargumente liest

Um Befehlszeilenargumente in Javascript zu lesen, gibt es die `process.argv` Funktion. Diese gibt uns ein Array mit allen Argumenten zurück, die bei der Ausführung des Programms angegeben wurden. Ein Beispiel hierfür wäre:

```javascript
const args = process.argv;

console.log(args);
```

Wenn wir dieses Programm mit dem Befehl `node index.js arg1 arg2 arg3` ausführen, wird das Console-Log folgende Ausgabe produzieren:

```
["node", "index.js", "arg1", "arg2", "arg3"]
```

Wie du sehen kannst, enthält das Array alle Argumente, die wir angegeben haben, einschließlich des Namens des ausführbaren Programms (`node`). Wir können nun auf die einzelnen Argumente zugreifen, indem wir auf die entsprechenden Indizes im Array zugreifen.

```javascript
const args = process.argv;

console.log("Das erste Argument ist: " + args[2]);
console.log("Das zweite Argument ist: " + args[3]);
console.log("Das dritte Argument ist: " + args[4]);
```

Die Ausgabe wird dann wie folgt aussehen:

```
Das erste Argument ist: arg1
Das zweite Argument ist: arg2
Das dritte Argument ist: arg3
```

##Tiefere Einblicke

Neben dem einfachen Einlesen von Argumenten mit `process.argv`, gibt es auch Möglichkeiten, die Argumente zu manipulieren und zu verarbeiten. Eine davon ist die Verwendung von `yargs`, einem npm-Paket, das uns bei der Verarbeitung von Befehlszeilenargumenten hilft.

Hier ist ein einfaches Beispiel, wie wir `yargs` in unser Programm einbinden können und wie es uns helfen kann, die eingegebenen Argumente zu verarbeiten:

```javascript
const yargs = require('yargs');

const argv = yargs.argv;

console.log("Die Summe deiner eingegebenen Argumente ist: " + (argv.arg1 + argv.arg2 + argv.arg3));
```

Wir können nun unser Programm mit dem Befehl `node index.js --arg1=2 --arg2=4 --arg3=6` ausführen und erhalten als Ausgabe:

```
Die Summe deiner eingegebenen Argumente ist: 12
```

Dieses Beispiel zeigt, wie wir mit Hilfe von `yargs` die Argumente direkt zu Zahlen konvertieren und sie dann weiter verarbeiten können.

##Siehe auch

- [Offizielle Dokumentation zur Verwendung von `process.argv`](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Npm-Paket `yargs`](https://www.npmjs.com/package/yargs)
- [Tutorial zur Verwendung von `yargs`](https://www.digitalocean.com/community/tutorials/nodejs-getting-started-with-yargs)

Viel Spaß beim Lesen und Programmieren!