---
title:                "TypeScript: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist eine wichtige Fähigkeit für jeden Programmierer und kann in verschiedenen Situationen nützlich sein. Zum Beispiel kann es verwendet werden, um Benutzereingaben zu verarbeiten oder um die Ausführung eines Programms basierend auf bestimmten Bedingungen zu steuern. In diesem Blog-Beitrag werden wir uns damit beschäftigen, wie man Befehlszeilenargumente in TypeScript liest.

## Wie man Befehlszeilenargumente in TypeScript liest

Um Befehlszeilenargumente in TypeScript zu lesen, müssen wir das Process-Modul aus dem Node.js-Framework verwenden. Dieses Modul bietet eine Vielzahl von Methoden, die uns dabei helfen, verschiedene Systeminformationen zu erhalten, einschließlich der Befehlszeilenargumente.

Um das Process-Modul in unserem TypeScript-Code zu verwenden, müssen wir es zuerst importieren:

```TypeScript
import process from 'process';
```

Als nächstes können wir die Methode `argv` verwenden, um eine Array von Befehlszeilenargumenten abzurufen:

```TypeScript
const args = process.argv;
```

Die `argv`-Methode gibt ein Array zurück, das sowohl den Pfad zum ausführbaren Programm als auch alle übergebenen Befehlszeilenargumente enthält. Wenn wir zum Beispiel unser Programm mit dem folgenden Befehl ausführen:

```
node index.ts arg1 arg2 arg3
```

Dann würde das Array `args` folgende Werte haben:

```TypeScript
['/usr/local/bin/node', '/path/to/nodejs/index.ts', 'arg1', 'arg2', 'arg3']
```

Wir können dann auf die einzelnen Befehlszeilenargumente wie auf jedes andere Array-Element zugreifen.

```TypeScript
console.log(`Erstes Argument: ${args[2]}`);
```

In diesem Beispiel würden wir "arg1" in der Konsole ausgeben.

## Tiefergehende Informationen

Es gibt noch viele weitere Methoden im Process-Modul, die uns dabei helfen können, Befehlszeilenargumente in TypeScript zu lesen. Hier sind ein paar weitere hilfreiche Beispiele:

- `cwd()` gibt den aktuellen Arbeitsverzeichnis-Pfad zurück.
- `execPath` gibt den Pfad zur ausführbaren Datei des aktuellen Prozesses zurück.
- `env` gibt ein Objekt zurück, das alle aktuellen Umgebungsvariablen enthält.

Weitere Informationen und Beispiele finden Sie in der offiziellen Dokumentation von Node.js.

## Siehe auch

- Offizielle Node.js-Dokumentation: https://nodejs.org/api/process.html
- TypeScript-Befehlszeilenargumente verarbeiten: https://www.tutorialspoint.com/typescript/typescript_command_line_arguments.htm
- Process-Modul-Referenz: https://www.w3schools.com/nodejs/ref_process.asp