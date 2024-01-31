---
title:                "Schreiben auf Standardfehler"
date:                  2024-01-19
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"

category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben auf Standard Error (stderr) ist der Prozess, Fehlermeldungen und Diagnoseinformationen in einem separaten Stream auszugeben. Programmierer nutzen dies, um Fehlernachrichten vom regulären Output zu trennen, wodurch die Anwendungslogik klar von der Fehlerbehandlung getrennt bleibt.

## How to:
Elm ist eine rein funktionale Sprache, die für Web-Frontend-Entwicklung ausgelegt ist. Da Elm-Code im Browser läuft, gibt es keine native Möglichkeit, direkt auf stderr zu schreiben wie in serverseitigen Sprachen. Für Debugging-Zwecke kann man jedoch die `Debug`-Module verwenden oder JavaScript-Interoperabilität durch Ports, um Nachrichten an `console.error` zu senden.

```Elm
port module Main exposing (..)

-- Definiere einen Port, um Nachrichten an JavaScript zu senden
port error : String -> Cmd msg

-- Funktion, die einen Fehler sendet
reportError : String -> Cmd msg
reportError errorMessage =
    error errorMessage

-- Beispiel einer Main-Funktion, die einen Fehler meldet
main =
    reportError "Etwas ist schiefgelaufen!"
```

JavaScript-Teil zum Empfangen der Nachricht:

```javascript
app.ports.error.subscribe(function(errorMessage) {
    console.error(errorMessage);
});
```

Absichtlich hat Elm keine direkte stderr-Unterstützung, um die Funktionen einfach und auf Frontend-Use-Cases fokussiert zu halten.

## Deep Dive
In traditionellen Programmiersprachen wie C oder Python wird stderr verwendet, um Fehlermeldungen unabhängig von stdout zu halten. Da man oft stdout umleitet oder in Dateien schreibt, hilft stderr dabei, Fehlermeldungen sichtbar und konsistent zu halten. In Elm gibt es aufgrund des Browser-Kontexts keine konzeptionelle Unterscheidung zwischen stdout und stderr. Dennoch kann man über JavaScript-Ports und die Browserkonsole ähnliche Funktionalitäten bereitstellen, wenn nötig.

Alternativen zur Fehlerbehandlung in Elm könnten das Wrapper-Type-Pattern sein, wo man Ergebnistypen (`Result`) verwendet, um Erfolge und Fehler in der Anwendungslogik darzustellen. Dies hält die Fehlerbehandlung funktional und eng integriert in den Elm-Code.

## See Also
- Elm's offizielle `Debug` Dokumentation: [https://package.elm-lang.org/packages/elm/core/latest/Debug](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- Elm's offizielle Guide zu Ports für JavaScript-Interoperabilität: [https://guide.elm-lang.org/interop/ports.html](https://guide.elm-lang.org/interop/ports.html)
- Elm's `Result` Typ, ein alternativer Fehlerbehandlungsansatz: [https://package.elm-lang.org/packages/elm/core/latest/Result](https://package.elm-lang.org/packages/elm/core/latest/Result)
