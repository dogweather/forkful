---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Lesen von Kommandozeilenargumenten kann ein Programm Eingaben direkt von der Konsole oder dem Terminal empfangen. Dies wird von Programmierern oft genutzt um Programme flexibler und interaktiver zu gestalten.

## So geht's:

Das Lesen von Befehlszeilenargumenten ist unkompliziert. Let's do it!

```TypeScript
let myArgs = process.argv.slice(2);
console.log('meine Argumente: ', myArgs);
```

Wenn Sie den obigen Code mit einigen Argumenten ausführen, gibt er diese Argumente aus. 

Führen Sie beispielsweise den folgenden Befehl aus:

```bash
node myScript.ts argument1 argument2
```

Dann ist die Ausgabe:

```bash
meine Argumente:  [ 'argument1', 'argument2' ]
```

## Deep Dive

Das Konzept der Kommandozeilenargumente ist nichts Neues und hat seinen Ursprung in den frühen Tagen der textbasierten Benutzeroberflächen. Es bleibt eine beliebte Praxis aufgrund seiner Direktheit und Einfachheit.

Alternativen zum Lesen von Kommandozeilenargumenten sind: Dateieingabe/ausgabe (IO), Benutzereingabe während der Laufzeit und Netzwerkanfragen, je nach Anwendungsfall.

Im Detail verwendet Node.js (und damit TypeScript) das `process.argv` Array für Zugriff auf die Befehlszeilenargumente. Das erste Element ist der Pfad zur Node.js ausführbaren Datei, das zweite Element ist der Pfad zur ausführbaren .ts Datei und der Rest des Arrays beinhaltet die eigentlichen Argumente.

## Siehe Auch

- [Node.js Dokumentation zu 'process.argv'](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [TypeScript Handbuch](https://www.typescriptlang.org/docs/)
- [Wertvoller Artikel über Kommandozeilenargumente](https://stackabuse.com/command-line-arguments-in-node-js/)