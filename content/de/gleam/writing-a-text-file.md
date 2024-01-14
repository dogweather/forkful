---
title:    "Gleam: Schreiben einer Textdatei"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Textdateien ist eine grundlegende Fähigkeit in der Programmierung, die es ermöglicht, Daten in einer lesbaren Form zu speichern und zu verarbeiten.

## Wie geht das?

Das Erstellen einer Textdatei in Gleam ist ziemlich einfach. In den folgenden Beispielen verwenden wir die Funktion `File.write` aus dem Modul `gleam/io`.

```Gleam
let result = File.write("mein_text.txt", "Dies ist ein Beispieltext.")
```

Dieses Beispiel würde eine Datei mit dem Namen "mein_text.txt" im aktuellen Ordner erstellen und den gegebenen Text in die Datei schreiben. Um zu überprüfen, ob die Datei erfolgreich erstellt wurde, können wir `result` in der Konsole ausgeben.

```Gleam
debug
  result
```

Die Ausgabe sollte `Ok` sein, um anzuzeigen, dass die Datei erfolgreich erstellt wurde.

## In die Tiefe

Beim Schreiben von Textdateien in Gleam gibt es einige zusätzliche Dinge zu beachten. Hier sind einige nützliche Tipps und Tricks:

- Wenn die Datei bereits existiert, wird der vorhandene Inhalt durch den neuen Inhalt überschrieben.
- Um mehrere Zeilen zu schreiben, können Sie den `\n` Zeilenumbruch verwenden.
- Sie können auch Variablen oder komplexere Datentypen in die Datei schreiben, indem Sie die Funktion `to_string` verwenden.

## Siehe auch

- [Gleam Dokumentation](https://gleam.run/documentation/)
- [Gleam Datei Eingabe/Ausgabe Modul](https://gleam.run/modules/io/)