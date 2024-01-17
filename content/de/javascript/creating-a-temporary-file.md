---
title:                "Erstellen einer temporären Datei"
html_title:           "Javascript: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Was? & Warum?

Das Erstellen einer temporären Datei ist eine gängige Praxis unter Programmierern, bei der eine Datei für einen begrenzten Zeitraum erstellt und verwendet wird. Dies kann hilfreich sein, um bestimmte Aufgaben auszuführen, die nur vorübergehend benötigt werden, wie z.B. das Speichern von Zwischenergebnissen oder das Ausführen von Tests.

# Wie geht's?

```Javascript
function createTempFile() {
  const tempFileName = 'myTempFile.txt';
  const tempFileContent = 'This is a sample temporary file.';
  // Creates a new temporary file with the specified name and content.
  fs.writeFileSync(tempFileName, tempFileContent);
  console.log(`Temporary file "${tempFileName}" successfully created.`);
}
createTempFile();
```
Ausgabe:
```
Temporary file "myTempFile.txt" successfully created.
```

# Tief tauchen

Das Erstellen von temporären Dateien wird schon seit Jahren von Programmierern verwendet. Früher war es üblich, manuell einen eindeutigen Dateinamen zu erstellen und die Datei zu löschen, wenn sie nicht mehr benötigt wurde. Heutzutage gibt es jedoch viele Bibliotheken und Frameworks, die das Erstellen von temporären Dateien vereinfachen und automatisieren.

Eine alternative Methode zum Erstellen von temporären Dateien ist die Verwendung von Speicherorten wie dem Arbeitsspeicher oder der Registrierung. Diese können jedoch begrenzte Größen haben und sind nicht so zuverlässig wie das Erstellen einer tatsächlichen Datei.

Die Implementierung des Erstellens von temporären Dateien kann je nach verwendeter Programmiersprache oder Bibliothek variieren. Es ist wichtig, sich mit der Dokumentation vertraut zu machen und sicherzustellen, dass die erstellten temporären Dateien ordnungsgemäß gelöscht werden.

# Siehe auch

- [fs.writeFileSync() Dokumentation](https://nodejs.org/api/fs.html#fs_fs_writefilesync_file_data_options)
- [tmp-promise Bibliothek](https://www.npmjs.com/package/tmp-promise)
- [Node.js für Anfänger – Dateien erstellen](https://www.tutorialspoint.com/nodejs/nodejs_file_system.htm)