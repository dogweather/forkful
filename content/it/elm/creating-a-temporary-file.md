---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

La creazione di un file temporaneo implica la realizzazione di un insieme di dati che verrà utilizzato solo in modo transitorio o che può essere eliminato in modo sicuro dopo l'utilizzo. I programmatori creano file temporanei per salvare i dati durante le sessioni l'utente o per le operazioni batch che hanno bisogno di uno storage temporaneo.

## Come fare:

Purtroppo, Elm non offre un modo diretto per creare file temporanei, in quanto è un linguaggio focalizzato sull'interfaccia utente lato client e non ha accesso diretto al sistema di archiviazione file. Puoi, tuttavia, inviare richieste a un server esterno che può creare file temporanei per te.

```Elm
type alias Model =
    { serverResponse : String }

type Msg
    = FileCreated String

update : Msg -> Model -> Model
update msg model =
    case msg of
        FileCreated response ->
            { model | serverResponse = response }

showResults : Model -> Html Msg
showResults model =
    div []
        [ text model.serverResponse ]

-- Puoi inviare una richiesta HTTP qui con Http.post o Http.get
```

## Approfondimenti:

Elm non offre funzionalità per la creazione di file temporanei, poiché la sua filosofia è incentrata sulla sicurezza e la prevenzione degli effetti collaterali, che significano comandi che potrebbero comportare un cambiamento al di fuori dello stato dell'app Elm. Questo deriva dal fatto che Elm è un linguaggio funzionale che si concentra sul concetto di "purezza", suscitando un evoluzione nella programmazione web in termini di sicurezza e prevedibilità. Tuttavia, potresti risolvere questa limitazione interagendo con JavaScript tramite i porti o sfruttando le possibilità offerte dai server.

## Vedi anche:

1. Documentazione ufficiale Elm: [https://elm-lang.org/docs](https://elm-lang.org/docs)
2. Uso dei porti in Elm: [https://guide.elm-lang.org/interop/ports.html](https://guide.elm-lang.org/interop/ports.html)
3. Interfacciare Elm con server: [https://elm-lang.org/guide/reactivity#talking-to-servers](https://elm-lang.org/guide/reactivity#talking-to-servers)
4. Tutorial HTTP in Elm: [https://elmprogramming.com/sending-http-requests.html](https://elmprogramming.com/sending-http-requests.html)