---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "TypeScript: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was und Warum? 
Das Lesen von Befehlszeilenargumenten ist eine Technik, die es Programmierern ermöglicht, Eingaben durch die Befehlszeile zu erhalten. Dies ist besonders nützlich, um Benutzereingaben zu verarbeiten oder den Programmablauf zu steuern.

## So geht's: 
Um Befehlszeilenargumente in TypeScript zu lesen, können wir die process.argv-Variable verwenden. Hier ist ein Beispiel, das die Befehlszeilenargumente ausgibt:

```TypeScript
const args = process.argv; 
console.log("Dieses Programm wurde mit den folgenden Befehlszeilenargumenten aufgerufen: ", args);
```

Beim Ausführen des obigen Codes mit dem Befehl `ts-node index.ts arg1 arg2` wird folgende Ausgabe erzeugt:

```
Dieses Programm wurde mit den folgenden Befehlszeilenargumenten aufgerufen: ["/usr/bin/node", "index.ts", "arg1", "arg2"]
```
## Tiefgehende Einblicke:
Das Lesen von Befehlszeilenargumenten ist eine häufig verwendete Praxis in der Programmierung. Es ermöglicht den Benutzern, Eingaben zur Laufzeit an ein Programm zu übergeben. Eine Alternative zum Lesen von Befehlszeilenargumenten ist das Lesen von Benutzereingaben über eine Benutzeroberfläche.

Die Implementierung von Befehlszeilenargumenten in TypeScript erfolgt über die process.argv-Variable, die ein Array von Befehlszeilenargumenten enthält. Diese Technik wird sowohl in plattformübergreifenden als auch in serverseitigen Anwendungen verwendet.

## Siehe auch:
- [Offizielle Dokumentation zu process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Eine Einführung in die Befehlszeilenargumente in TypeScript](https://www.twilio.com/blog/2017/10/reading-command-line-arguments-in-typescript.html)
- [Ein Tutorial zur Verwendung von Befehlszeilenargumenten in Node.js-Anwendungen](https://www.digitalocean.com/community/tutorials/how-to-use-node-js-cli-arguments)