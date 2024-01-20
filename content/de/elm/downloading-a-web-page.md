---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Was und Warum?
Das Herunterladen einer Webseite ist eine Möglichkeit, Daten von einem Webserver zu beziehen. Programmierer tun dies, um Webinhalte zu analysieren, Abläufe zu automatisieren oder Daten für späteres Offline-Lesen zu speichern.

# So geht's:
Leider gibt es in Elm (aktueller Stand: Version 0.19) keine direkte Möglichkeit, eine Webseite herunterzuladen. Elm fokussiert sich hauptsächlich auf Frontend-Entwicklung und Benutzerinteraktionen hatten immer Priorität. Deshalb gibt es keine eingebauten Funktionen für Serverinteraktionen wie Dateidownloads. Aber Elm kann mit Javascript interagieren, mit dessen Hilfe Serverinteraktionen möglich sind. Mit einem Port könnten wir eine Nachricht an Javascript senden, um eine Datei herunterzuladen.

```Elm
port module Main exposing (..)

type alias Model =
    { url : String }

init : Model
init = {
    url = "https://example.com"
}

port download : String -> Cmd msg
```

Dann könnten Sie diesen Code in Ihrer Javascript-Datei verwenden, um auf die Nachricht zu hören und den Download zu starten:

```Javascript
app.ports.download.subscribe(function(url) {
    var link = document.createElement('a');
    link.href = url;
    link.download = 'download';
    link.click();
});
```

Letztendlich ist es nicht besonders "elmisch", aber es funktioniert.

# Vertiefung
(1) Historischer Kontext:
In früheren Versionen von Elm, war es noch etwas schwieriger, mit Ports zu arbeiten und Webseiten herunterzuladen.
(2) Alternativen:
Als Alternative könnten Sie eine serverseitige Sprache wie Node.js, Python oder PHP verwenden, um Webseiten herunterzuladen und zu speichern.
(3) Umsetzungsdetails:
In Elm lösen wir das Problem durch Kommunikation mit Javascript über Ports.

# Siehe auch
- Offizielle Elm-Website: https://elm-lang.org/
- Elm Guide über Ports: https://guide.elm-lang.org/interop/ports.html
- Diskussion zum Download von Dateien in Elm: https://discourse.elm-lang.org/t/downloading-files-in-elm/2246