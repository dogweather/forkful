---
title:                "Das Lesen von Befehlszeilenargumenten"
html_title:           "TypeScript: Das Lesen von Befehlszeilenargumenten"
simple_title:         "Das Lesen von Befehlszeilenargumenten"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Es gibt verschiedene Gründe, warum jemand Interesse an der Lektüre von Befehlszeilenargumenten haben könnte. Zum einen können sie in der Entwicklung von Anwendungen sehr nützlich sein, um die Ausführung des Programms zu steuern. Zum anderen können sie auch für Tests oder Fehlerbehebungszwecke verwendet werden.

## Wie geht das

Um Befehlszeilenargumente in TypeScript zu lesen, können wir das `process.argv` Array verwenden. Dieses Array enthält alle Argumente, die beim Aufrufen des Programms über die Befehlszeile übergeben werden. Um die Argumente auszugeben, können wir eine Schleife verwenden, die durch das Array geht und jedes Argument ausgibt.

```TypeScript
// Beispiel zum Auslesen von Befehlszeilenargumenten
for (let i = 2; i < process.argv.length; i++) {
    console.log(`Argument ${i - 1}: ${process.argv[i]}`);
}
```

Wenn wir nun das Programm mit dem Befehl `node index.js argument1 argument2` aufrufen, wird folgende Ausgabe angezeigt:

```bash
Argument 1: argument1
Argument 2: argument2
```

Wir können auch überprüfen, ob ein bestimmtes Argument vorhanden ist, indem wir das Array nach diesem Argument durchsuchen. Wenn das Argument gefunden wird, gibt `process.argv.indexOf(argument)` den Index des Arguments im Array zurück. Ist das Argument nicht vorhanden, wird `-1` zurückgegeben.

## Tiefer Einblick

Das `process.argv` Array enthält nicht nur die Argumente, die über die Befehlszeile übergeben werden, sondern auch einige andere Informationen wie den Pfad zum ausführbaren Skript oder den Pfad zum aktuellen Verzeichnis. Dies kann besonders nützlich sein, wenn wir ein Skript schreiben, das sowohl auf Linux- als auch auf Windows-Systemen ausgeführt werden soll, da sich der Pfad zum ausführbaren Skript auf den verschiedenen Systemen unterscheiden kann.

Es ist auch wichtig zu beachten, dass die Argumente als Strings im Array gespeichert werden, unabhängig davon, ob sie als Zahlen oder Booleans übergeben wurden. Wenn wir beabsichtigen, sie als andere Datentypen zu verwenden, müssen wir sie entsprechend konvertieren.

## Siehe auch 

- [Node.js `process.argv` Dokumentation](https://nodejs.org/api/process.html#process_process_argv) 
- [Befehlszeilenargumente in TypeScript auslesen](https://stackoverflow.com/questions/4351521/how-to-pass-command-line-arguments-to-node-js) 
- [Offizielle TypeScript Dokumentation](https://www.typescriptlang.org/docs/)