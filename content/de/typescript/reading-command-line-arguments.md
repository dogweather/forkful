---
title:    "TypeScript: Lesen von Befehlszeilenargumenten"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumemten ist ein wichtiger Bestandteil der TypeScript-Programmierung. Es ermöglicht es, Variablen und Einstellungen direkt beim Start des Programms anzugeben, anstatt sie zuvor im Code festzulegen. Dadurch wird das Programm flexibler und kann für verschiedene Zwecke verwendet werden.

## Wie

Um Befehlszeilenargumente in TypeScript zu lesen, können wir das Argumentenarray `process.argv` verwenden. Dieses enthält alle vom Benutzer beim Start des Programms angegebenen Argumente. Zum Beispiel:

```TypeScript
const argument1 = process.argv[2];
const argument2 = process.argv[3];

console.log(argument1); // Ausgabe: Wert des ersten Arguments
console.log(argument2); // Ausgabe: Wert des zweiten Arguments
```

Es ist jedoch zu beachten, dass das erste Element von `process.argv` immer der Pfad zum ausführbaren Programm ist und die Argumente erst ab dem zweiten Element starten. Um dies zu berücksichtigen, sollten wir das Array mit `slice(2)` verkleinern, um die tatsächlichen Argumente zu erhalten.

```TypeScript
const argumente = process.argv.slice(2);
```

## Tiefergehende Informationen

Neben dem Lesen von einfachen Argumentenwerten, können wir auch mit komplexeren Argumenten umgehen, wie zum Beispiel mit Flaggen. Diese kennzeichnen bestimmte Eigenschaften oder Einstellungen des Programms und können als `true` oder `false` interpretiert werden.

```TypeScript
const flag = process.argv.includes("--flag");

if (flag) {
  // Flagge ist vorhanden
  // Führe entsprechende Aktion aus
}
```

Es ist auch möglich, optionale Argumente zu definieren, indem wir in unserem Programm verschiedene Fälle abfragen. Zum Beispiel könnten wir ein Argument definieren, welches eine Datei als Eingabe erwartet. Ist dieses Argument nicht vorhanden, könnten wir stattdessen eine Standarddatei verwenden.

```TypeScript
const eingabedatei = process.argv[2] || "standarddatei.txt";
```

## Siehe auch

- [Dokumentation zu process.argv von Node.js](https://nodejs.org/api/process.html#process_process_argv)
- [Artikel über Befehlszeilenargumente in TypeScript](https://hackernoon.com/command-line-arguments-in-typescript-9db33c92bdc4)
- [Einführung in die TypeScript-Programmierung auf Deutsch](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)