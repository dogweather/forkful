---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Javascript: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum
Bist du bereit, deine Javascript-Kenntnisse auf die nächste Stufe zu bringen? Dann bist du hier genau richtig! In diesem Artikel lernst du, wie du Befehlszeilenargumente in deinem Javascript-Code lesen und nutzen kannst.

# Wie es geht
Um Befehlszeilenargumente in Javascript zu lesen, müssen wir die `process.argv`-Methode verwenden. Diese Methode gibt ein Array zurück, das alle Argumente enthält, die bei der Ausführung des Skripts in der Befehlszeile angegeben wurden. Ein Beispielcode könnte so aussehen:
```
Javascript
// Beispiel für process.argv
console.log(process.argv);

// Beispiel-Output wenn der folgende Befehl in der Befehlszeile ausgeführt wird:
// node readArguments.js hello world
// Output:
// [ 'node', 'readArguments.js', 'hello', 'world' ]
```
Wie du sehen kannst, gibt es zwei vordefinierte Argumente in dem Array: `node`, welches den Pfad zu deinem Node.js-Programm enthält, und der Dateiname deines Skripts. Alle weiteren Argumente, die nach dem Dateinamen angegeben werden, werden in diesem Array gespeichert.

Du kannst auch einzelne Argumente aus dem Array extrahieren, indem du ihren Index verwendest. Zum Beispiel, um das erste eingegebene Argument "hello" zu erhalten, könntest du `process.argv[2]` verwenden.

# Deep Dive
Um das Lesen von Befehlszeilenargumenten besser zu verstehen, ist es wichtig zu wissen, dass `process.argv` Teil des "process" Objekts in Node.js ist. Dieses Objekt enthält viele nützliche Methoden und Informationen über den aktuellen Prozess.

Um Befehlszeilenargumente noch effektiver zu nutzen, kannst du auch das `yargs`-Modul verwenden. Dieses Modul erleichtert das Parsen und Validieren von Befehlszeilenargumenten und bietet zusätzliche Funktionen wie die Unterstützung von Abkürzungen und Aliasen für Argumente.

# Siehe auch
- [Node.js-Dokumentation für process.argv](https://nodejs.org/api/process.html#process_process_argv)
- [yargs GitHub-Repository](https://github.com/yargs/yargs)