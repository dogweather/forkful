---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:17.972536-07:00
description: "Elm non supporta nativamente YAML, un formato di serializzazione dei\
  \ dati spesso utilizzato per file di configurazione o condivisione di dati, a causa\
  \ del\u2026"
lastmod: '2024-03-13T22:44:43.369882-06:00'
model: gpt-4-0125-preview
summary: "Elm non supporta nativamente YAML, un formato di serializzazione dei dati\
  \ spesso utilizzato per file di configurazione o condivisione di dati, a causa del\u2026"
title: Lavorare con YAML
---

{{< edit_this_page >}}

## Cos'è e Perché?
Elm non supporta nativamente YAML, un formato di serializzazione dei dati spesso utilizzato per file di configurazione o condivisione di dati, a causa del suo forte enfasi sulla sicurezza dei tipi e risultati prevedibili. Tuttavia, i programmatori incontrano frequentemente YAML quando si occupano di API o configurazioni nello sviluppo web, rendendo necessari metodi affidabili per analizzare i dati YAML nell'ecosistema strettamente tipizzato di Elm per una integrazione e manipolazione senza problemi.

## Come fare:
Per gestire YAML in Elm, è tipicamente necessario convertire YAML in JSON al di fuori di Elm e poi utilizzare la funzionalità di decodifica JSON integrata in Elm per lavorare con i dati. Sebbene questo approccio richieda un ulteriore passaggio di conversione, sfrutta il robusto sistema di tipi di Elm per garantire l'integrità dei dati. Gli strumenti popolari per la conversione da YAML a JSON includono convertitori online o servizi backend. Una volta che hai JSON, puoi utilizzare il modulo `Json.Decode` di Elm per lavorare con i dati.

Prima, assumendo di avere i seguenti dati YAML:

```yaml
person:
  name: Jane Doe
  age: 30
```

Convertirli in formato JSON:

```json
{
  "person": {
    "name": "Jane Doe",
    "age": 30
  }
}
```

Quindi, definisci il tuo modello e decodificatore Elm:

```elm
module Main exposing (..)

import Html exposing (text)
import Json.Decode as Decode

type alias Person =
    { name : String
    , age : Int
    }

decoderPersona : Decode.Decoder Person
decoderPersona =
    Decode.map2 Person
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

```

Per utilizzare questo decodificatore per convertire JSON in un tipo Elm:

```elm
import Json.Decode as Decode

jsonString = 
    """
    {
      "person": {
        "name": "Jane Doe",
        "age": 30
      }
    }
    """

risultatoDecodifica = Decode.decodeString (Decode.field "person" decoderPersona) jsonString

main =
    case risultatoDecodifica of
        Ok person ->
            Html.text ("Ciao, " ++ person.name ++ "!")
            
        Err _ ->
            Html.text "Si è verificato un errore durante la decodifica."
```

Output (renderizzato in un'applicazione Elm):
```
Ciao, Jane Doe!
```

Questo approccio assicura che tu possa lavorare con dati YAML in Elm utilizzando JSON come formato intermediario, sfruttando il solido sistema di tipi di Elm e le capacità di decodifica JSON per manipolare in modo sicuro ed efficace dati esterni.
