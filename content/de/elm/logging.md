---
title:                "Protokollierung"
date:                  2024-01-26T01:02:00.076068-07:00
model:                 gpt-4-1106-preview
simple_title:         "Protokollierung"

category:             "Elm"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/logging.md"
---

{{< edit_this_page >}}

## Was & Warum?
Protokollierung ist im Wesentlichen der Prozess der Aufzeichnung von Ereignissen und Datenausgaben einer Software während ihres Betriebs – man kann sie sich wie das Tagebuch der Software vorstellen. Programmierer nutzen die Protokollierung, um den Überblick darüber zu behalten, was unter der Haube passiert – sie ist äußerst wertvoll für das Debugging, die Überwachung des Systemverhaltens in Echtzeit und die Analyse vergangener Aktivitäten zur Leistungsoptimierung oder für Audits.

## Wie geht das:
Elms Architektur unterstützt Seiteneffekte wie Protokollierung nicht direkt – diese werden durch Befehle gehandhabt, die ein Teil der Architektur Ihrer Anwendung sind. Zu Lehrzwecken schauen wir uns an, wie Sie Protokollierung simulieren könnten, indem Sie über Ports Nachrichten an JavaScript senden.

Zuerst definieren Sie ein Port-Modul:

```Elm
port module Logger exposing (..)

-- Definieren Sie einen Port, um Log-Meldungen an JavaScript zu senden
port log : String -> Cmd msg
```

In Ihrer `Main.elm` würden Sie den `log`-Port verwenden, um eine Protokollnachricht zu senden:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- hier einige Aktualisierungen Ihres Modells
            ( updatedModel, log "AnEvent occurred." )

        AnotherEvent ->
            -- andere Modellaktualisierungen hier
            ( anotherUpdatedModel, log "AnotherEvent occurred." )
```

Auf der JavaScript-Seite würden Sie den `log`-Port abonnieren, um die eingehenden Protokollnachrichten zu verarbeiten:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

Eine beispielhafte Ausgabe in der JavaScript-Konsole wäre dann:

```
AnEvent occurred.
AnotherEvent occurred.
```

## Vertiefung
In Sprachen wie Python oder Java wird die Protokollierung üblicherweise durch die Verwendung einer Protokollierungsbibliothek durchgeführt, welche eine einfache API zum Protokollieren von Nachrichten auf verschiedenen Ebenen wie Debug, Info, Warnung, Fehler und Kritisch bietet.

Elm, mit seinem Fokus auf Reinheit und Unveränderlichkeit, bietet diese Art der direkten Protokollierung nicht an, da jede Art von IO oder Seiteneffekt deutlich durch die Elm-Architektur verwaltet wird.

Wenn Sie eine vollwertige Protokollierung in Elm benötigen, verlassen Sie sich typischerweise auf externe JavaScript-Tools. Ports, wie oben gezeigt, sind die Brücke zu diesen Werkzeugen. Das Debug-Modul ist eine weitere Option, aber es ist nur für die Entwicklung gedacht und nicht für die Protokollierung in der Produktion.

Neben Ports nutzen Programmierer oft die Compiler-Nachrichten von Elm und Laufzeit-Debugging-Einrichtungen, wie `Debug.log`, das Sie in Ihren Code einfügen können, um Werte nachzuverfolgen. Es umhüllt einen Ausdruck und protokolliert dessen Ausgabe in der Konsole, wie folgt:

```Elm
view model =
    Debug.log "Model Debug" model
    -- Ihr View-Code hier
```

Dies ist jedoch auch nicht für die Produktion gedacht. Werkzeuge wie elm-logger bieten einige Abstraktionen über Ports für die Protokollierung, obwohl diese auch mehr für die Entwicklung als für die Produktion gedacht sind.

## Siehe Auch
- Elm-Ports: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Elm-Diskurs über Protokollierung: https://discourse.elm-lang.org/t/elm-and-logging/546
- JavaScript Console API: https://developer.mozilla.org/de/docs/Web/API/Console
- elm-logger Paket: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
