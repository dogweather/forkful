---
title:                "TypeScript: Lesen von Befehlszeilenargumenten"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Argumenten von der Befehlszeile ist ein wesentlicher Bestandteil der Programmierung in TypeScript. Es ermöglicht es Entwicklern, Benutzereingaben zu erfassen und diese in ihren Programmen zu verwenden. Ohne dieses Wissen könnte man keine interaktiven Programme erstellen.

## So geht's

Um Befehlszeilenargumente in TypeScript zu lesen, müssen wir die `process` Modul von Node.js verwenden. Zuerst müssen wir es in unserem Programm importieren:

```TypeScript
import * as process from 'process';
```

Als nächstes können wir die `process.argv` Eigenschaft verwenden, um alle übergebenen Argumente zu erhalten. Diese Eigenschaft gibt ein Array mit Strings zurück, wobei das erste Element immer der Name des ausführbaren Programms ist. Zum Beispiel, wenn wir unser Programm mit `node main.ts argument1 argument2` aufrufen, würde `process.argv` folgendes zurückgeben:

```TypeScript
[
  '/usr/local/bin/node',
  '/Users/user/Documents/main.ts',
  'argument1',
  'argument2'
]
```

Um auf bestimmte Argumente zuzugreifen, können wir einfach die jeweilige Indexposition im Array angeben. Zum Beispiel, um auf `argument1` zuzugreifen, können wir `process.argv[2]` verwenden. Beachten Sie, dass die Indexposition `0` für den Programmnamen reserviert ist.

Wir können auch die `slice()` Methode verwenden, um nur die Argumente zu erhalten, die wir benötigen, ohne den Programmnamen. Zum Beispiel, `process.argv.slice(2)` würde ein Array mit den Argumenten `argument1` und `argument2` zurückgeben.

## Tiefere Einblicke 

Neben dem Lesen von Befehlszeilenargumenten können wir auch überprüfen, ob bestimmte Argumente vorhanden sind oder nicht. Dazu können wir die `includes()` Methode verwenden, die es seit ES2016 gibt. Zum Beispiel können wir überprüfen, ob unser Programm mit dem `--help` Argument aufgerufen wurde:

```TypeScript
if (process.argv.includes('--help')) {
  console.log('Hilfe-Option wurde aufgerufen.');
}
```

Das Lesen von Befehlszeilenargumenten kann auch in Kombination mit anderen Modulen verwendet werden, um komplexe Programme zu erstellen. So könnte man zum Beispiel mit dem `yargs` Modul noch mehr Funktionalitäten und Möglichkeiten beim Lesen von Argumenten erhalten.

## Siehe auch

- [Node.js `process` Dokumentation](https://nodejs.org/api/process.html)
- [yargs Modul](https://github.com/yargs/yargs)