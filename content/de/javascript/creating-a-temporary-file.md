---
title:                "Javascript: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit der Erstellung von temporären Dateien beschäftigen? Die Antwort ist einfach - temporäre Dateien sind eine hilfreiche Möglichkeit, um Daten während der Ausführung eines Programms temporär zu speichern. Sie können nützlich sein, um die Leistung zu verbessern oder bestimmte Aufgaben zu automatisieren.

## Wie funktioniert es?

Um eine temporäre Datei in Javascript zu erstellen, gibt es mehrere Möglichkeiten. Eine davon ist die Verwendung der Native Module API, beispielsweise das "fs" Modul. Mit dieser Methode kann eine leere temporäre Datei erstellt werden, die dann mit Inhalt gefüllt werden kann. Hier ist ein Beispielcode:

```Javascript
const fs = require('fs');
fs.open('temp.txt', 'w', (err, file) => {
  if (err) throw err;
  console.log('Temporary file created!');
});
```

Dieser Code erstellt eine leere temporäre Datei mit dem Namen "temp.txt" und führt dann eine Konsolenausgabe aus. Es ist wichtig zu beachten, dass diese Methode die Datei nicht automatisch löscht - das muss manuell erfolgen.

Eine andere Möglichkeit ist die Verwendung des "tmp" Moduls aus der Node.js Standardbibliothek. Dieses Modul bietet eine einfachere Möglichkeit, eine temporäre Datei zu erstellen und wieder zu löschen. Hier ist ein Beispielcode:

```Javascript
const tmp = require('tmp');
const tempFile = tmp.fileSync();
console.log(tempFile.name);
tempFile.removeCallback();
```

Dieser Code erstellt eine temporäre Datei und gibt ihren Namen aus, bevor sie automatisch gelöscht wird.

## Tiefer Einblick

Beim Erstellen einer temporären Datei gibt es einige Dinge zu beachten. Zunächst ist es wichtig, eine eindeutige Dateinamenstruktur zu wählen, um Konflikte mit anderen Dateien zu vermeiden. Auch das Löschen der Datei ist entscheidend, um den Speicher nicht unnötig zu belasten.

Darüber hinaus ist es wichtig zu wissen, wie temporäre Dateien vom Betriebssystem behandelt werden. In einigen Fällen können sie automatisch gelöscht werden, während sie in anderen Fällen möglicherweise manuell entfernt werden müssen. Es ist daher ratsam, die Dokumentation des jeweiligen Betriebssystems zu konsultieren, um ein besseres Verständnis zu erhalten.

## Siehe auch

- [Node.js fs Modul Dokumentation](https://nodejs.org/api/fs.html)
- [Node.js tmp Modul Dokumentation](https://nodejs.org/api/fs.html)
- [Verwendung temporärer Dateien in Javascript](https://www.digitalocean.com/community/tutorials/how-to-use-temporary-files-in-javascript) (Englisch)