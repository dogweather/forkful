---
title:                "Registrazione Eventi (Logging)"
aliases:
- it/elm/logging.md
date:                  2024-01-26T01:02:12.853815-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registrazione Eventi (Logging)"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/logging.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Il logging è essenzialmente il processo di registrazione di eventi e output di dati da un software durante il suo funzionamento, pensalo come il diario del software. I programmatori utilizzano il logging per tenere traccia di ciò che sta succedendo sotto il cofano - è inestimabile per il debugging dei problemi, il monitoraggio del comportamento del sistema in tempo reale e l'analisi delle attività passate per ottimizzazioni delle prestazioni o audit.

## Come fare:
L'architettura di Elm non supporta effetti collaterali come il logging di base—li gestisci attraverso comandi, che sono una parte dell'architettura della tua applicazione. A scopo didattico, vediamo come potresti simulare il logging inviando messaggi a JavaScript tramite porte (ports).

Prima, definirai un modulo port:

```Elm
port module Logger exposing (..)

-- Definisce una port per inviare log a JavaScript
port log : String -> Cmd msg
```

Nel tuo `Main.elm`, utilizzeresti la porta `log` per inviare un messaggio di log:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- alcuni aggiornamenti al tuo modello qui
            ( updatedModel, log "Si è verificato AnEvent." )

        AnotherEvent ->
            -- altri aggiornamenti al modello qui
            ( anotherUpdatedModel, log "Si è verificato AnotherEvent." )
```

Sul lato JavaScript, ti iscriveresti alla porta `log` per gestire i messaggi di log in arrivo:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

Un esempio di output nella console JavaScript sarebbe quindi:

```
Si è verificato AnEvent.
Si è verificato AnotherEvent.
```

## Approfondimento
Tradizionalmente, in linguaggi come Python o Java, il logging viene eseguito utilizzando una libreria di logging, che fornisce un'API semplice per registrare messaggi a vari livelli quali debug, info, warning, error e critical.

Elm, con il suo focus sulla purezza e sull'immutabilità, non fornisce questo tipo di logging diretto, poiché ogni tipo di IO o effetto collaterale è gestito distintamente attraverso l'architettura di Elm.

Quando hai bisogno di un logging completo in Elm, normalmente ti affidi a strumenti JavaScript esterni. Le porte, come mostrato sopra, sono il ponte verso questi strumenti. Il modulo Debug è un'altra opzione, ma è destinato solo all'uso in fase di sviluppo e non per il logging in produzione.

In aggiunta alle porte, i programmatori spesso sfruttano i messaggi del compilatore di Elm e le facilitazioni di debugging in runtime, come `Debug.log`, che puoi inserire nel tuo codice per tracciare i valori. Avvolge un'espressione e registra il suo output nella console così:

```Elm
view model =
    Debug.log "Debug del Modello" model
    -- il tuo codice della view qui
```

Tuttavia, anche questo non è destinato alla produzione. Strumenti come elm-logger forniscono alcune astrazioni sulle porte per il logging, sebbene anche questi siano destinati più allo sviluppo che alla produzione.

## Vedi anche
- Elm ports: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Discussioni su Elm e logging: https://discourse.elm-lang.org/t/elm-and-logging/546
- JavaScript Console API: https://developer.mozilla.org/en-US/docs/Web/API/Console
- Pacchetto elm-logger: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
