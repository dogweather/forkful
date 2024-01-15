---
title:                "Eine Textdatei lesen"
html_title:           "TypeScript: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Wenn Sie ein Entwickler sind, der mit TypeScript arbeitet, werden Sie früher oder später auf die Aufgabe stoßen, eine Textdatei zu lesen. Das kann aus verschiedenen Gründen notwendig sein, zum Beispiel um Konfigurationsdaten zu laden oder Eingabedaten zu verarbeiten. In diesem Artikel zeige ich Ihnen, wie Sie mithilfe von TypeScript eine Textdatei lesen können.

# Wie geht das?

Um eine Textdatei mit TypeScript zu lesen, benötigen Sie zunächst eine Datei mit dem Text, den Sie lesen möchten. Nehmen wir als Beispiel die folgende Datei `sample.txt`:

```
Hallo, dies ist eine Beispieltextdatei.
Auf mehreren Zeilen können auch mehrere Sätze stehen.
```

Um die Datei zu lesen, können Sie die integrierte `readFileSync`-Funktion von TypeScript verwenden, die Teil des `fs`-Moduls ist. Diese Funktion erwartet den Pfad zur Datei als Parameter und gibt den Inhalt der Datei als String zurück.

```
```TypeScript
import * as fs from 'fs';

const fileContent = fs.readFileSync('sample.txt', 'utf-8');
console.log(fileContent);
```

Die Ausgabe dieser Codezeilen wird sein:

```
Hallo, dies ist eine Beispieltextdatei.
Auf mehreren Zeilen können auch mehrere Sätze stehen.
```

# Tiefeneintauch

Die `readFileSync`-Funktion von TypeScript bietet mehrere Optionen, die Sie nutzen können, um die Art und Weise zu steuern, wie die Datei gelesen wird. Einige davon sind:

- `encoding` - Hiermit können Sie das Encoding der gelesenen Datei festlegen, standardmäßig ist es `utf-8`.
- `flag` - Mit dieser Option können Sie angeben, ob die Datei nur gelesen oder auch beschrieben werden soll. Standardmäßig ist die Flag auf `r` gesetzt, also `read-only`.

Wenn Sie mehr Kontrolle über den Lesevorgang benötigen, können Sie auch die `createReadStream`-Funktion verwenden, die ein `ReadStream`-Objekt zurückgibt, welches Sie dann weiter verarbeiten können.

# Siehe auch

- [Das `fs`-Modul von Node.js](https://nodejs.org/api/fs.html)
- [Das `ReadStream`-Objekt](https://nodejs.org/api/fs.html#fs_fs_createreadstream_path_options)