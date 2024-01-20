---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen einer Textdatei bedeutet, die darin gespeicherten Daten auszulesen und für das Programm verfügbar zu machen. Programmierer machen das, um Zugang zu externen Informationen zu haben und diese innerhalb ihrer Anwendungen zu verwenden.

## So geht’s:

Elm (Version 0.19.1 und höher) ist eine funktionale Sprache, die noch keine direkten Möglichkeiten bietet, Dateien zu lesen. Aber sie interagiert gut mit JavaScript. Daher könnten wir die JavaScript-Funktion für das Lesen einer Datei verwenden und dann die Daten an Elm übergeben. 

Erstens, fügen wir einige JavaScript-Code hinzu:

```JavaScript
var app = Elm.Main.init(); 
var reader = new FileReader();

reader.onload = function(){
  app.ports.readFile.send(reader.result); 
};

function readfile(e) {
  var file = e.target.files[0];
  if(!file) return;
  reader.readAsText(file);
}
```

Der obige JS-Code ermöglicht das Lesen von Dateien und sendet dann die Daten über einen Port an unser Elm-Programm. Dann fügen wir den Port zu unserem Elm-Programm hinzu:

```Elm
port module Main exposing (..)

type alias Model = 
     { file : Maybe String 
     }
     
port readFile : (String -> msg) -> Sub msg
```
Jetzt erhält die Anwendung alle Dateidaten auf der Elm-Seite.

## Vertiefung:

Historisch gesehen waren die Möglichkeiten zum Ein- und Auslesen von Dateien in funktionalen Programmiersprachen wie Elm immer etwas komplizierter. Das liegt daran, dass die meisten funktionalen Sprachen versuchen, Nebenwirkungen wie Dateioperationen zu vermeiden.

Es gibt auch andere Möglichkeiten, Dateien in Elm zu lesen, wie zum Beispiel das Einbetten der Dateidaten zur Kompilierzeit durch Webpack oder ähnliche Werkzeuge. Letztendlich interessieren uns jedoch die Daten, die wir aus der Datei erhalten können, und durch welches Medium wir auf sie zugreifen, ist nebensächlich.

## Siehe auch:

- Elm Ports Tutorial: https://guide.elm-lang.org/interop/ports.html
- FileReader API-Methode in JavaScript: https://developer.mozilla.org/de/docs/Web/API/FileReader
- Einbetten von Assets mit Webpack: https://webpack.js.org/guides/asset-management/