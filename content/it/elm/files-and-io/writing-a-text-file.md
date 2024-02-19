---
aliases:
- /it/elm/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:54.979915-07:00
description: "Scrivere un file di testo in Elm comporta la creazione e il salvataggio\
  \ di dati testuali in un file da un'applicazione Elm. Spesso, ai programmatori \xE8\
  \u2026"
lastmod: 2024-02-18 23:08:55.823525
model: gpt-4-0125-preview
summary: "Scrivere un file di testo in Elm comporta la creazione e il salvataggio\
  \ di dati testuali in un file da un'applicazione Elm. Spesso, ai programmatori \xE8\
  \u2026"
title: Scrivere un file di testo
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere un file di testo in Elm comporta la creazione e il salvataggio di dati testuali in un file da un'applicazione Elm. Spesso, ai programmatori è necessario generare report, log o esportare dati in un formato di testo strutturato (ad esempio, JSON, CSV) per l'utilizzo in altre applicazioni o per fini di archiviazione. Tuttavia, a causa dell'architettura di Elm che si concentra sulla purezza e sulla sicurezza, la scrittura diretta di file - come molti altri effetti collaterali - è gestita tramite comandi all'ambiente JavaScript circostante.

## Come fare:

Poiché Elm viene eseguito nel browser ed è progettato per essere un linguaggio di programmazione puro senza effetti collaterali, non ha accesso diretto al file system. Pertanto, scrivere su un file tipicamente comporta l'invio dei dati a JavaScript tramite porte (ports). Ecco come puoi configurare questo processo:

1. **Definisci un modulo port per inviare testo a JavaScript:**

```elm
port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- Definisci una porta per inviare dati di testo a JavaScript
port saveText : String -> Cmd msg

-- Visualizzazione principale
view : Html msg
view =
    div []
        [ button [ onClick (saveText "Ciao, Elm scrive su un file!") ] [ text "Salva su File" ]
        ]

-- Configurazione di abbonamento (non utilizzata in questo esempio ma necessaria per un modulo port)
subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none

-- Configurazione dell'applicazione
main : Program () model msg
main =
    Browser.element
        { init = \_ -> ((), Cmd.none)
        , view = \_ -> view
        , update = \_ _ -> ((), Cmd.none)
        , subscriptions = subscriptions
        }
```

2. **Implementa il corrispondente codice JavaScript:**

Nel tuo file HTML o in un modulo JavaScript, gestisci la porta dell'applicazione Elm per salvare il testo. Potresti usare la libreria `FileSaver.js` per salvare il file lato client oppure inviare i dati a un server per l'elaborazione.

```javascript
// Supponendo che Elm.Main.init() sia già stato chiamato e l'app sia in esecuzione
app.ports.saveText.subscribe(function(text) {
    // Utilizzando FileSaver.js per salvare i file lato client
    var blob = new Blob([text], {type: "text/plain;charset=utf-8"});
    saveAs(blob, "esempio.txt");
});
```

L'output di esempio non è direttamente applicabile poiché il risultato è la creazione di un file, ma dopo aver cliccato il pulsante nella tua applicazione Elm, dovrebbe essere scaricato sul tuo computer un file denominato "esempio.txt" contenente la stringa "Ciao, Elm scrive su un file!".

In questo approccio, la comunicazione tra Elm e JavaScript è essenziale. Sebbene Elm miri a contenere quanto più possibile la logica della tua applicazione, l'interoperabilità con JavaScript tramite porte ti consente di eseguire compiti come la scrittura di file che Elm non supporta direttamente. Ricorda, la purezza e la sicurezza di Elm sono potenziate da questo schema, garantendo che le tue applicazioni Elm rimangano facili da mantenere e da comprendere, anche quando interagiscono con il complesso mondo esterno.
