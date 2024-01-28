---
title:                "Lavorare con TOML"
date:                  2024-01-26T04:21:13.527795-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con TOML"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/working-with-toml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
TOML, acronimo di Tom's Obvious, Minimal Language, è un linguaggio di serializzazione dei dati. I programmatori Elm lo utilizzano per gestire i dati di configurazione perché è facilmente leggibile dall'uomo e mappa ordinatamente le coppie chiave-valore necessarie nelle applicazioni.

## Come fare:
Elm non dispone di un parser TOML integrato, ma è possibile interagire con JavaScript o utilizzare un pacchetto della comunità. Ecco come si potrebbe analizzare TOML usando un ipotetico pacchetto `elm-toml`:

```elm
import Toml

configToml : String
configToml =
    """
    [server]
    port = 8080
    """

parseResult : Result Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

Per decodificare valori specifici:

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

L'output di esempio per `port` potrebbe essere `Ok 8080` se la decodifica ha successo.

## Approfondimento
TOML è stato creato da Tom Preston-Werner, cofondatore di GitHub, come un linguaggio semplice per i file di configurazione. Compete con YAML e JSON; la sintassi di TOML mira al meglio di entrambi i mondi con un focus sulla facilità di lettura e scrittura da parte degli umani.

In Elm, per gestire TOML, di solito è necessario passare attraverso l'interop con JavaScript, il che può essere un po' complicato. Fortunatamente, la comunità Elm è piena di risorse e esistono diversi pacchetti di terze parti. L'ipotetico pacchetto `elm-toml` probabilmente utilizzerebbe il `Port` di Elm per comunicare con un parser TOML in JavaScript o implementerebbe direttamente l'analisi in Elm.

L'ostacolo principale in Elm è che tutto è tipizzato staticamente, quindi sarà necessario scrivere decodificatori personalizzati per gestire diverse strutture di dati all'interno di TOML, il che può essere un po' verboso ma aggiunge sicurezza.

## Vedi Anche
Per le specifiche e più informazioni su TOML stesso, consulta [TOML](https://toml.io).
Se stai cercando un approccio pratico all'interopabilità tra Elm e JavaScript, inizia con la guida ufficiale: [Porte di Elm](https://guide.elm-lang.org/interop/ports.html).
Per i pacchetti della comunità o per contribuire, sfoglia [Pacchetti Elm](https://package.elm-lang.org/).
