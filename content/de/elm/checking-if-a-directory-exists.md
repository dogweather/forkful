---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Elm: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

In Programmiersprachen überprüfen wir oft, ob ein Verzeichnis existiert, da es uns hilft, Fehler wie das Fehlen einer erforderlichen Datei zu vermeiden. Dies kann dazu beitragen, die Stabilität und Zuverlässigkeit unseres Programmes zu gewährleisten.

## So geht's:

Elm hat nicht die native Fähigkeit, auf Dateisysteme zuzugreifen, aber es kann durch Ports auf JavaScript zugreifen. So ein Beispiel könnte folgendermaßen aussehen:

```Elm
port module Main exposing (..)

port checkDirectoryExists : String -> Cmd msg
port directoryExists : (Bool -> msg) -> Sub msg
```

Dann würden Sie auf der JS-Seite den Callback `directoryExists` anwenden:

```JavaScript
const app = Elm.Main.init();
const fs = require('fs');

app.ports.checkDirectoryExists.subscribe((dir) => {
  const dirExists = fs.existsSync(dir);
  app.ports.directoryExists.send(dirExists);
});
```

Die Ausgabe wäre dann `True` wenn das Verzeichnis existiert, oder `False` wenn es nicht existiert.

## Vertiefung

Historisch gesehen wurde Elm für Web-Anwendungen entwickelt und hatte dementsprechend keinen eingebauten Zugriff auf Dateisysteme. Dies kann über Ports mit JavaScript umgangen werden, es ist jedoch wichtig zu beachten, dass dies aufgrund Sicherheitseinschränkungen von Browsern nur auf der Serverseite oder in einer Umgebung wie NodeJS durchgeführt werden kann.

Alternativ können Sie auch Tools wie Elixir und Phoenix mit Elm verwenden, die eine komfortable Server-Seiten Umgebung bieten, in welcher Sie Verzeichnisse und Dateien überprüfen können.

## Siehe auch

Für mehr zu diesem Thema, siehe folgende Links:

- [Elm Ports Dokumentation](https://guide.elm-lang.org/interop/ports.html)
- [NodeJS Dateisystem API](https://nodejs.org/api/fs.html)