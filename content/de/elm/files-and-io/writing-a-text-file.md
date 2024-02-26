---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:05.246788-07:00
description: "Das Schreiben einer Textdatei in Elm beinhaltet das Erstellen und Speichern\
  \ von textuellen Daten in einer Datei aus einer Elm-Anwendung heraus.\u2026"
lastmod: '2024-02-25T18:49:50.885101-07:00'
model: gpt-4-0125-preview
summary: "Das Schreiben einer Textdatei in Elm beinhaltet das Erstellen und Speichern\
  \ von textuellen Daten in einer Datei aus einer Elm-Anwendung heraus.\u2026"
title: Eine Textdatei schreiben
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben einer Textdatei in Elm beinhaltet das Erstellen und Speichern von textuellen Daten in einer Datei aus einer Elm-Anwendung heraus. Programmierer müssen häufig Berichte, Logs oder exportierte Daten in einem strukturierten Textformat (z.B. JSON, CSV) generieren, um sie in anderen Anwendungen zu verwenden oder zu Dokumentationszwecken zu speichern. Jedoch wird das direkte Schreiben von Dateien - wie viele andere Seiteneffekte auch - aufgrund der auf Reinheit und Sicherheit fokussierten Architektur von Elm, durch Befehle an die umgebende JavaScript-Umgebung gehandhabt.

## Wie zu:

Da Elm im Browser läuft und als reine Programmiersprache ohne Seiteneffekte konzipiert ist, hat es keinen direkten Zugriff auf das Dateisystem. Daher beinhaltet das Schreiben in eine Datei typischerweise das Senden der Daten an JavaScript durch Ports. Hier erfahren Sie, wie Sie dies einrichten können:

1. **Definieren Sie ein Port-Modul zum Senden von Text an JavaScript:**

```elm
port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- Definieren Sie einen Port, um Textdaten an JavaScript zu senden
port saveText : String -> Cmd msg

-- Hauptansicht
view : Html msg
view =
    div []
        [ button [ onClick (saveText "Hallo, Elm schreibt in eine Datei!") ] [ text "In Datei speichern" ]
        ]

-- Setup für Abonnements (nicht verwendet in diesem Beispiel, aber erforderlich für ein Port-Modul)
subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none

-- Anwendungseinrichtung
main : Program () model msg
main =
    Browser.element
        { init = \_ -> ((), Cmd.none)
        , view = \_ -> view
        , update = \_ _ -> ((), Cmd.none)
        , subscriptions = subscriptions
        }
```

2. **Implementieren Sie den entsprechenden JavaScript-Code:**

In Ihrer HTML-Datei oder einem JavaScript-Modul, bearbeiten Sie den Port der Elm-Anwendung zum Speichern des Textes. Sie könnten die Bibliothek `FileSaver.js` nutzen, um die Datei clientseitig zu speichern oder die Daten an einen Server zur Verarbeitung zu senden.

```javascript
// Unter der Annahme, dass Elm.Main.init() bereits aufgerufen wurde und die App läuft
app.ports.saveText.subscribe(function(text) {
    // Verwenden von FileSaver.js, um Dateien auf der Client-Seite zu speichern
    var blob = new Blob([text], {type: "text/plain;charset=utf-8"});
    saveAs(blob, "beispiel.txt");
});
```

Eine direkte Beispiel-Ausgabe ist hier nicht anwendbar, da das Resultat die Erstellung einer Datei ist, aber nach dem Klicken auf den Button in Ihrer Elm-Anwendung sollte eine Datei namens "beispiel.txt" mit dem String "Hallo, Elm schreibt in eine Datei!" auf Ihren Computer heruntergeladen werden.

Bei diesem Ansatz ist die Kommunikation zwischen Elm und JavaScript wesentlich. Obwohl Elm darauf abzielt, so viel Logik Ihrer Anwendung wie möglich zu beinhalten, ermöglicht das Interop mit JavaScript durch Ports Ihnen, Aufgaben wie das Schreiben von Dateien durchzuführen, die Elm nicht direkt unterstützt. Denken Sie daran, dass die Reinheit und Sicherheit von Elm durch dieses Muster verstärkt werden, und gewährleisten Sie, dass Ihre Elm-Anwendungen auch bei der Interaktion mit der komplexen Außenwelt leicht zu warten und zu durchschauen sind.
