---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# JavaScript: Lesen von Befehlszeilenargumenten

## Was & Warum?
Befehlszeilenargumente sind Werte, die an Ihre Skripte weitergegeben werden, wenn Sie sie ausführen. Mit ihnen können Sie dynamische Skripte erstellen, die sich je nach übergebenen Werten unterschiedlich verhalten.

## So geht's:

In Node.js können wir auf Befehlszeilenargumente über das Array `process.argv` zugreifen. Schauen wir uns ein einfachen Beispiel an:

```Javascript
// befArg.js
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});
```

Führen Sie jetzt das Skript mit einigen Argumenten aus:

```bash
$ node befArg.js eins zwei drei
```

Sie erhalten folgende Ausgabe:

```bash
0: /usr/local/bin/node
1: /Users/YourName/befArg.js
2: eins
3: zwei
4: drei
```

## In die Tiefe:

**Historischer Kontext:**
Das Konzept der Befehlszeilenargumente ist so alt wie die Befehlszeile selbst und existiert in fast allen Programmiersprachen.

**Alternativen:**
Für komplexere Anwendungen könnten Sie Bibliotheken wie `commander`, `yargs` oder `minimist` verwendet, die erweiterte Parsing-Funktionen anbieten. Sie könnten auch Umgebungsvariablen nutzen.

**Implementierungsdetails:**
Beachten Sie, dass `process.argv` mehr enthält als nur die Argumente, die Sie übergeben. Die ersten beiden Einträge sind der Pfad zu `node` und der Pfad zu Ihrem Skript. Ihre Argumente starten ab der Indexposition 2.

## Siehe auch:

- Node.js Dokumentation - Process: https://nodejs.org/api/process.html 
- Commander.js: https://www.npmjs.com/package/commander
- Yargs: https://www.npmjs.com/package/yargs
- Minimist: https://www.npmjs.com/package/minimist