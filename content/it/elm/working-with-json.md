---
title:                "Elm: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se stai iniziando a imparare Elm, probabilmente hai sentito parlare di JSON e ti chiedi perché dovresti preoccuparti di lavorarci. JSON (JavaScript Object Notation) è un formato di dati leggibile dagli umani, che viene comunemente utilizzato per lo scambio di informazioni tra server e client web. Utilizzando Elm, possiamo facilmente gestire e analizzare i dati JSON per creare applicazioni web dinamiche e interattive.

## Come Fare

Per lavorare con JSON in Elm, è necessario importare il modulo `Json.Decode` e utilizzare la funzione `decodeValue` per decodificare un valore JSON in un tipo di dati Elm. Ad esempio, se abbiamo un valore JSON che rappresenta un oggetto "persona" con i campi "nome" e "cognome", possiamo codificarlo in questo modo:

```elm
import Json.Decode exposing (..)

type alias Person =
    { name : String
    , surname : String
    }

personDecoder : Decoder Person
personDecoder =
    decode Person
        |> required "name" string
        |> required "surname" string

```

Una volta definito il decodificatore, possiamo utilizzarlo per trasformare il valore JSON in un tipo di dati Elm, ad esempio:

```elm
person : Result String Person
person =
    decodeValue personDecoder myJson
```

Se il valore JSON è valido, otterremo un oggetto `Ok` contenente i dati decodificati, altrimenti otterremo un oggetto `Err` con un messaggio di errore.

## Approfondimento

Esistono molti altri metodi e funzioni utili per lavorare con JSON in Elm, come la gestione delle date e la decodifica di valori più complessi. È importante anche tenere presente che alla base di Elm c'è un sistema di tipizzazione forte, che aiuta a prevenire possibili errori di codifica a runtime.

Vale la pena dedicare del tempo per studiare la documentazione ufficiale di Elm sul modulo `Json.Decode` e sperimentare con diversi tipi di dati e casi d'uso. Inoltre, è possibile utilizzare strumenti di terze parti come `elm-decode-pipeline` per semplificare e rendere più leggibile la decodifica dei valori JSON.

## Vedi Anche

- Documentazione ufficiale di Elm sul working con JSON (https://guide.elm-lang.org/effects/json.html)
- `elm-decode-pipeline` (https://github.com/NoRedInk/elm-decode-pipeline)