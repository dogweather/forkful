---
title:                "Javascript: Lesen von Kommandozeilenargumenten"
simple_title:         "Lesen von Kommandozeilenargumenten"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist sehr praktisch, um die Interaktivität und Flexibilität eines JavaScript Programms zu erhöhen. Indem man Parameter direkt in der Befehlszeile eingibt, kann man das Verhalten des Codes beeinflussen und anpassen, ohne das Programm jedes Mal neu schreiben zu müssen.

## Wie man Befehlszeilenargumente liest

Das Lesen von Befehlszeilenargumenten ist in JavaScript relativ einfach und kann mit nur wenigen Codezeilen implementiert werden. Um eine Argumentvariabele zu erstellen, muss man die `process.argv` Funktion aufrufen. Diese gibt ein Array zurück, das alle Argumente enthält, die vom Benutzer in der Befehlszeile eingegeben wurden.

```Javascript
const arguments = process.argv;
```

Um auf einzelne Argumente zuzugreifen, muss man einfach auf das entsprechende Index in dem Array zugreifen. Der Index 0 enthält dabei den Pfad zur Node.js-Installation und der Index 1 den Pfad zur ausführbaren JavaScript-Datei. Alle weiteren Argumente beginnen ab Index 2.

```Javascript
// Beispielaufruf: node script.js argument1 argument2

console.log(arguments[2]); // gibt 'argument1' aus
console.log(arguments[3]); // gibt 'argument2' aus
```

## Tieferer Einblick

Neben dem Lesen von einzelnen Argumenten gibt es auch die Möglichkeit, Optionen oder Flags bei der Eingabe zu verwenden. Diese können zusätzliche Informationen für das Programm enthalten und sind häufig durch das Vorzeichen "-" oder "--" gekennzeichnet.

Um auf solche Optionen zuzugreifen, bietet es sich an, eine Schleife zu verwenden, die durch das gesamte `process.argv` Array läuft. Dabei sollte man Wert auf eine gute Fehlerbehandlung legen, da fehlende oder falsch formatierte Argumente zu unerwarteten Fehlern führen können.

```Javascript
// Beispielaufruf: node script.js --name Max --age 25

for (let i = 2; i < arguments.length; i++) {
    if (arguments[i] === '--name') {
        // gibt den Wert der Option --name aus
        console.log(arguments[i + 1]); // gibt 'Max' aus
    } else if (arguments[i] === '--age') {
        // gibt den Wert der Option -- age als Integer aus
        console.log(parseInt(arguments[i + 1])); // gibt 25 aus
    }
}
```

## Siehe auch
- [Node.js process.argv Documentation](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [How to Read Command Line Arguments in Node.js](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-node-js-de) (Englisch)
- [Commander.js: Library for creating CLI programs in Node.js](https://github.com/tj/commander.js) (Englisch)