---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:44.281865-07:00
description: "Lavorare con JSON in Elm riguarda la decodifica dei dati JSON in tipi\
  \ Elm e la codifica di valori Elm di nuovo in JSON. Questo processo \xE8 cruciale\
  \ per le\u2026"
lastmod: '2024-02-25T18:49:41.238051-07:00'
model: gpt-4-0125-preview
summary: "Lavorare con JSON in Elm riguarda la decodifica dei dati JSON in tipi Elm\
  \ e la codifica di valori Elm di nuovo in JSON. Questo processo \xE8 cruciale per\
  \ le\u2026"
title: Lavorare con JSON
---

{{< edit_this_page >}}

## Cosa e Perché?
Lavorare con JSON in Elm riguarda la decodifica dei dati JSON in tipi Elm e la codifica di valori Elm di nuovo in JSON. Questo processo è cruciale per le applicazioni web per interagire con API e fonti di dati esterne, consentendo uno scambio di dati senza soluzione di continuità tra il client (Elm) e server o altri servizi.

## Come fare:

Elm tratta la gestione JSON con esplicitezza e sicurezza, utilizzando principalmente i moduli `Json.Decode` e `Json.Encode`. Per iniziare a lavorare con JSON, devi prima definire un decodificatore per il tuo tipo di dato. Supponiamo che ci stiamo occupando di un semplice oggetto profilo utente.

Innanzitutto, definisci il tuo tipo Elm:

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### Decodificare JSON in Elm

Per decodificare una stringa JSON nel tipo `UserProfile`, crea un decodificatore:

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

Per decodificare un oggetto JSON:

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- Output di esempio:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### Codificare Elm in JSON

Per codificare un valore Elm di nuovo in JSON, sfrutta il modulo `Json.Encode`.

```elm
import Json.Encode exposing (object, int, string)

encodeUserProfile : UserProfile -> String
encodeUserProfile userProfile =
    object
        [ ("id", int userProfile.id)
        , ("name", string userProfile.name)
        , ("email", string userProfile.email)
        ]
        |> Json.Encode.encode 0

{- 
Uso:
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

Output di esempio:
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### Librerie di Terze Parti

Pacchetti Elm come `elm-json-decode-pipeline` possono semplificare la creazione di decodificatori usando uno stile pipeline, che è particolarmente utile per decodificare oggetti complessi.

Prima, aggiungi la libreria al tuo progetto:

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

Poi, puoi semplificare la definizione del decodificatore come segue:

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- Usa questo decodificatore come prima con decodeString per decodificare stringhe JSON. -}
```

Questo approccio semplifica il decodificatore, rendendo il codice più pulito e più manutenibile, specialmente man mano che le strutture dati diventano più complesse.
