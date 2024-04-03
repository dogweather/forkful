---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:21.353731-07:00
description: "Wie: Elm ist eine Frontend-Webprogrammiersprache und hat daher keinen\
  \ direkten Zugriff auf das Dateisystem. \xDCblicherweise w\xFCrde man einen Befehl\
  \ an einen\u2026"
lastmod: '2024-03-13T22:44:53.819599-06:00'
model: gpt-4-0125-preview
summary: Elm ist eine Frontend-Webprogrammiersprache und hat daher keinen direkten
  Zugriff auf das Dateisystem.
title: "\xDCberpr\xFCfung, ob ein Verzeichnis existiert"
weight: 20
---

## Wie:
Elm ist eine Frontend-Webprogrammiersprache und hat daher keinen direkten Zugriff auf das Dateisystem. Üblicherweise würde man einen Befehl an einen Backend-Service in JavaScript senden. So könnten Sie eine solche Interaktion mit Elm strukturieren:

```elm
port module Main exposing (..)

-- Einen Port definieren, um mit JavaScript zu kommunizieren
port checkDir : String -> Cmd msg

-- Beispielgebrauch
checkDirectory : String -> Cmd Msg
checkDirectory dir =
    checkDir dir
```

Dann in Ihrem JavaScript:

```javascript
app.ports.checkDir.subscribe(function(dir) {
    var exists = fs.existsSync(dir); // Das benutzt Nodes 'fs'-Modul, um das Verzeichnis zu überprüfen
    app.ports.dirExists.send(exists);
});
```

Zurück in Elm, die Antwort handhaben:

```elm
port dirExists : (Bool -> msg) -> Sub msg

type Msg = DirExists Bool

subscriptions : Model -> Sub Msg
subscriptions model =
    dirExists DirExists
```

Hinweis: Dies erfordert die Einrichtung von Ports und eine angemessene Backend-Behandlung in JavaScript.

## Tiefergehende Betrachtung
Elms auf den Browser beschränkte Umgebung bedeutet, dass es im Gegensatz zu Node.js nicht direkt auf das Dateisystem zugreifen kann. Historisch gesehen haben serverseitige Sprachen und Node.js Funktionalitäten für den Dateisystemzugriff bereitgestellt, während Browsersprachen auf Server-APIs angewiesen sind, um Dateien zu verwalten. Elms striktes Typsystem verwaltet keine Seiteneffekte wie E/A-Operationen nativ; stattdessen verwendet es Ports für die JavaScript-Interoperabilität. Obwohl Elm selbst nicht überprüfen kann, ob ein Verzeichnis existiert, ermöglicht die Verwendung von Elm mit einem Backend-Service über Ports diese Funktionalität in Webanwendungen.

Alternativen in einer Node.js-Umgebung umfassen die `fs.existsSync` oder `fs.access` Methoden. Für Elm betrachten Sie serverseitiges Elm mit einem Backend wie `elm-serverless`, das Dateioperationen direkter als clientseitiges Elm handhaben kann.

In Bezug auf die Implementierung, sobald Sie Ihre Ports eingerichtet haben, sendet Ihre Elm-App Nachrichten an JavaScript, das die Überprüfung des Dateisystems durchführt. JavaScript sendet dann die Ergebnisse zurück zu Elm. Dies hält Elms Frontend-Code rein und frei von Seiteneffekten, und bewahrt seine Architekturprinzipien.

## Siehe auch
- Elm Offizieller Leitfaden zu Ports: https://guide.elm-lang.org/interop/ports.html
- Node.js `fs` Moduldokumentation: https://nodejs.org/api/fs.html
- elm-serverless für serverseitige Elm-Interaktionen: https://package.elm-lang.org/packages/ktonon/elm-serverless/latest/
