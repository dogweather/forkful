---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:56:28.663923-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
"Was & Warum?" bezieht sich darauf, wie man in Elm überprüfen kann, ob ein Verzeichnis existiert. Dies ist wichtig, um Fehler zu verhindern, wenn man auf Dateien zugreift oder Operationen durchführt, die ein bestimmtes Verzeichnis voraussetzen.

## How to:
Elm läuft im Browser und hat keinen direkten Zugriff auf Dateisysteme. Daher können wir nicht direkt in Elm prüfen, ob ein Verzeichnis existiert. Stattdessen nutzen wir Javascript über Ports. Hier ist ein Beispiel, wie das gemacht wird.

Elm-Code, der eine Nachricht über einen Port sendet:

```Elm
port module Main exposing (..)

-- Definiere einen Port, um eine Nachricht an JavaScript zu senden.
port checkDirectory : String -> Cmd msg

-- Sende einen Befehl, um zu überprüfen, ob ein Verzeichnis existiert.
checkIfDirectoryExists : String -> Cmd msg
checkIfDirectoryExists directoryPath =
    checkDirectory directoryPath
```

Dazugehöriger JavaScript-Code, der den Port abhört:

```javascript
// Abonnieren des Ports aus Elm, um Verzeichnis-Checks zu handhaben.
app.ports.checkDirectory.subscribe(function(directoryPath) {
    // Überprüfen, ob das Verzeichnis existiert (pseudo-code).
    const directoryExists = fs.existsSync(directoryPath); // Node.js-Funktion
    // Sende das Ergebnis zurück zum Elm-Code.
    app.ports.directoryCheckResult.send(directoryExists);
});
```

## Deep Dive
Historisch gesehen ist Elm für die Verwendung im Browser konzipiert und hat daher keinen direkten Zugriff auf das Dateisystem eines Servers oder eines Clients. Um das zu umgehen, verwenden Entwickler Ports, um Nachrichten zwischen Elm und Javascript auszutauschen. Alternativen dazu könnten sein, eine reine JavaScript-Frontend-Lösung zu nutzen oder die Verzeichnisprüfung auf dem Server in einer Backend-Sprache wie Node.js durchzuführen.

Die Implementierung mittels Ports erfordert ein gutes Verständnis des Nachrichtenaustauschs zwischen Elm und JavaScript. Dabei ist zu beachten, dass die Kommunikation asynchron ist; man muss also Callbacks oder Promises in JavaScript nutzen, um mit den Ergebnissen umzugehen.

## See Also
Für weitere Informationen, siehe:

- Elm Ports Dokumentation: https://guide.elm-lang.org/interop/ports.html
- `fs.existsSync` Node.js Dokumentation: https://nodejs.org/api/fs.html#fsexistssyncpath
- Elm und JavaScript Interoperabilität: https://elm-lang.org/news/porting-to-elm