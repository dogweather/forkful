---
title:                "Elm: Ein neues Projekt beginnen"
simple_title:         "Ein neues Projekt beginnen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich auf ein neues Elm Projekt einlassen? Nun, Elm ist eine funktionale, statisch typisierte Sprache, die es ermöglicht, robuste und fehlerfreie Webanwendungen zu entwickeln. Mit seiner hilfreichen Compiler-Unterstützung und seiner klaren Syntax ist Elm eine großartige Wahl für Entwickler, die sauberen und skalierbaren Code schätzen.

## Wie es geht

Um ein neues Elm Projekt zu starten, müssen Sie zunächst die Elm-Toolchain auf Ihrem Computer installieren. Sie können dies über den Befehl ```npm install -g elm``` in der Kommandozeile tun. Sobald die Installation abgeschlossen ist, können Sie ein neues Verzeichnis für Ihr Projekt erstellen und in der Kommandozeile ```elm init``` eingeben. Daraufhin wird Elm eine grundlegende Projektstruktur erstellen, die Sie für Ihre Anwendung nutzen können.

Lassen Sie uns nun einen einfachen "Hello World" Beispielcode schreiben. Öffnen Sie dazu Ihre Lieblings-IDE und erstellen Sie eine neue Datei mit dem Namen "Main.elm".

```
module Main exposing (..)

import Html exposing (text)

main : Html msg
main =
    text "Hello, World!"
```

Wenn Sie jetzt in der Kommandozeile ```elm reactor``` eingeben und auf die generierte URL in Ihrem Browser zugreifen, sollten Sie die Ausgabe "Hello World" sehen.

## Tiefer tauchen

Wenn Sie Ihr Elm-Projekt in eine produktionsreife Anwendung verwandeln möchten, gibt es einige bewährte Methoden, die Sie befolgen können. Zum Beispiel empfehlen erfahrene Elm-Entwickler, die Entwicklung mit einem "Model-View-Update" Ansatz zu beginnen, um komplexe Anwendungen besser zu strukturieren. Außerdem können Sie von der Verwendung von Elm-Paketen profitieren, die Ihnen bereits implementierte Funktionen und UI-Komponenten bieten.

## Siehe auch

- Offizielle Elm-Website: [https://elm-lang.org/](https://elm-lang.org/)
- Elm-Paketverzeichnis: [https://package.elm-lang.org/](https://package.elm-lang.org/)
- Elm-Community-Foren: [https://discourse.elm-lang.org/](https://discourse.elm-lang.org/)