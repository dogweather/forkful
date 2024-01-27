---
title:                "Eine Textdatei schreiben"
date:                  2024-01-19
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Schreiben einer Textdatei bedeutet, Daten in eine lesbare Datei auf dem Datenträger zu speichern. Programmierer tun dies, um Daten dauerhaft zu archivieren, zu teilen oder Konfigurationen zu speichern.

## How to:
Elm selbst hat keinen direkten Weg, auf das Dateisystem zuzugreifen - das wird über JavaScript und Ports erledigt. 

```Elm
port module Main exposing (..)

-- Definiere einen Port zum Senden von Daten an JavaScript
port saveFile : String -> Cmd msg

-- Verwende den Port in einer Elm-Funktion
saveTextToFile : String -> Cmd msg
saveTextToFile text =
    saveFile text

-- Beispiel für Text, der geschrieben werden soll
exampleText : String
exampleText =
    "Hallo, das ist ein Beispieltext!"

-- Befiehlt Elm, den Text zu speichern
main : Cmd msg
main =
    saveTextToFile exampleText
```

In JavaScript würdest du den Port so abhören und die Datei speichern:

```JavaScript
app.ports.saveFile.subscribe(function(text) {
    // Benutze die FileSystem API oder ähnliches, um die Datei zu speichern
    console.log("Speichere Datei mit Text: ", text);
});
```

## Deep Dive
Elm ist eine funktionale Sprache für Frontend-Web-Entwicklung. Die fehlende Dateisystem-Unterstützung schränkt direkte File-Operationen ein, was aber Sicherheit und Einfachheit bietet. Alternativen wären die Nutzung von Web APIs wie `FileWriter` (im Browser) oder Serverseitige Elm-Ports mit Node.js für Back-End-Funktionalitäten. Implementierungsdetails variieren je nach Einsatzgebiet: Browsersicherheit oder serverseitige Logik bestimmen, wie man die Dateizugriffe handhabt.

## See Also
- Elm Ports Dokumentation: [https://guide.elm-lang.org/interop/ports.html](https://guide.elm-lang.org/interop/ports.html)
- FileSystem API MDN: [https://developer.mozilla.org/en-US/docs/Web/API/FileSystem](https://developer.mozilla.org/en-US/docs/Web/API/FileSystem)
