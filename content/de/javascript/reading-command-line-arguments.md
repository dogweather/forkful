---
title:    "Javascript: Lesen von Befehlszeilenargumenten"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Warum

In der Welt der Javascript-Programmierung gibt es viel zu lernen. Eine wichtige Fähigkeit ist das Lesen von Befehlszeilenargumenten, da diese es ermöglichen, Eingaben von Benutzern in eine Anwendung zu integrieren und sie in den Code zu implementieren.

##Wie man Befehlszeilenargumente liest

Das Lesen von Befehlszeilenargumenten ist in Javascript relativ einfach, erfordert jedoch ein gewisses Verständnis von Funktionen und Variablen. Zuerst müssen wir eine Variante anlegen, die die Befehlszeilenargumente abruft. Dann können wir diese Argumente in eine Funktion einsetzen, die sie analysiert.

```Javascript
let args = process.argv.slice(2);
args.forEach(function (arg) {
  console.log(arg);
});
```

In diesem Beispiel erstellen wir eine Variable namens "args" und rufen die Befehlszeilenargumente über das "process.argv"-Modul ab. Diese Befehlszeilenargumente werden dann in eine Funktion eingesetzt und können durch Laufen der Funktion einzeln ausgegeben werden. Wenn wir zum Beispiel den Befehl "node index.js 1 2 3" verwenden, würde die Ausgabe folgendermaßen aussehen:

```
1
2
3
```

##Tieferer Einblick

Es ist wichtig zu wissen, dass der Befehl "node" selbst auch ein Befehlszeilenargument ist und somit als erstes Element in der "args"-Variable gespeichert wird. Die restlichen Argumente werden dann als einzelne Elemente gespeichert und können durch Verwendung von "slice(2)" erreicht werden.

Eine weitere Sache, die zu beachten ist, ist die Möglichkeit, optionale Argumente zu implementieren. Dies kann durch das Überprüfen der Länge der "args"-Variable und das Setzen von Standardwerten für fehlende Argumente erreicht werden.

##Siehe auch

- https://www.digitalocean.com/community/tutorials/node-js-interactive-command-line-prompts
- https://nodejs.org/en/knowledge/command-line/how-to-parse-command-line-arguments/
- https://www.tutorialspoint.com/nodejs/nodejs_command_line_arguments.htm