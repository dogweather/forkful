---
title:    "Javascript: Erstellen einer temporären Datei"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung gibt es unzählige Anwendungen und Aufgaben, die wir erledigen müssen. Eine davon könnte das Erstellen temporärer Dateien sein. Doch warum sollte man überhaupt eine temporäre Datei erstellen wollen?

Eine mögliche Anwendung ist das Zwischenspeichern von Daten, die später weiterverarbeitet werden müssen. Auch bei der Ausführung von bestimmten Prozessen kann es notwendig sein, eine Zwischendatei zu erstellen, um das Ergebnis später verwenden zu können. Temporäre Dateien können auch dabei helfen, den Übersichtlichkeit und Ordnung auf dem Computer zu bewahren, da sie nach dem Gebrauch automatisch gelöscht werden.

Im Folgenden zeige ich euch, wie ihr mit Javascript eine temporäre Datei erstellen könnt und gebe Einblicke in die Funktionsweise.

## Wie geht das?

Die Erstellung von temporären Dateien kann in Javascript mit Hilfe von Node.js und dem "fs" Modul durchgeführt werden. Zuerst müssen wir das Modul in unserem Code einbinden, indem wir folgenden Code am Anfang unseres Skripts platzieren:

```javascript
const fs = require('fs');
```

Als nächstes müssen wir einen Dateinamen und den gewünschten Speicherort angeben, an dem die temporäre Datei erstellt werden soll. Dazu verwenden wir die Methode "fs.mkdtempSync()", die uns eine zufällig generierte eindeutige Zeichenkette als Dateiname zurückgibt. Beispiel:

```javascript
const tempFile = fs.mkdtempSync('myTempFile-', 'utf-8');
```

Die Datei wird nun mit dem angegebenen Namen und dem Suffix ".tmp" erstellt und kann wie jede andere Datei behandelt werden. Zum Beispiel können wir Daten in die Datei schreiben:

```javascript
fs.writeFileSync(tempFile, 'Hallo Welt!');
```

Um den Inhalt der Datei zu überprüfen, können wir diesen auslesen und in der Konsole ausgeben:

```javascript
const fileContent = fs.readFileSync(tempFile, 'utf-8');
console.log(fileContent); // Ausgabe: Hallo Welt!
```

Sobald das Programm beendet wird, wird die temporäre Datei automatisch gelöscht.

## Tiefere Einblicke

Beim Erstellen einer temporären Datei können verschiedene Parameter angegeben werden, um die Ausführungsweise anzupassen. Zum Beispiel kann die gewünschte Länge des generierten Dateinamens angegeben werden oder es können eigene Präfixe und Suffixe definiert werden. Dadurch wird die Datei noch eindeutiger und kann besser identifiziert werden.

Es gibt auch die Möglichkeit, die erstellten Dateien in einem spezifischen Ordner abzulegen, anstatt im standardmäßigen "tmp" Ordner. Dadurch kann eine bessere Kontrolle über die erstellten Dateien ausgeübt werden.

## Siehe auch

- [Node.js Dokumentation zu fs.mkdtempSync()](https://nodejs.org/api/fs.html#fs_fs_mkdtempsync_prefix_options)
- [Online File Converter für die Umwandlung von Dateien](https://www.online-convert.com/de)