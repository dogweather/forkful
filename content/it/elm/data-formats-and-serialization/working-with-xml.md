---
aliases:
- /it/elm/working-with-xml/
date: 2024-01-26 04:30:25.363273-07:00
description: "Lavorare con XML significa analizzare, trasformare e generare documenti\
  \ XML in Elm. Si fa per interagire con molti servizi web e sistemi legacy che usano\u2026"
lastmod: 2024-02-18 23:08:55.829391
model: gpt-4-0125-preview
summary: "Lavorare con XML significa analizzare, trasformare e generare documenti\
  \ XML in Elm. Si fa per interagire con molti servizi web e sistemi legacy che usano\u2026"
title: Lavorare con XML
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con XML significa analizzare, trasformare e generare documenti XML in Elm. Si fa per interagire con molti servizi web e sistemi legacy che usano XML come formato dei dati.

## Come fare:
In Elm, si gestisce l'XML utilizzando il pacchetto `elm/xml`. Ecco una rapida occhiata all'analisi di un frammento XML:

```Elm
import Xml.Decode exposing (..)
import Xml.Decode.Pipeline exposing (..)

xmlString = """
<book id="123">
    <title>Elm in Action</title>
    <author>Robin Heggelund Hansen</author>
</book>
"""

type alias Book =
    { id : String
    , title : String
    , author : String
    }

bookDecoder : Decoder Book
bookDecoder =
    decode Book
        |> required "id" (attribute "id")
        |> required "title" (child "title" (content text))
        |> required "author" (child "author" (content text))

case Xml.Decode.fromString bookDecoder xmlString of
    Ok book ->
        -- Fai qualcosa con il libro decodificato qui
        Debug.toString book

    Err error ->
        -- Gestisci errori
        Debug.toString error
```

Output di esempio, assumendo l'assenza di errori:

```Elm
"{ id = \"123\", title = \"Elm in Action\", author = \"Robin Heggelund Hansen\" }"
```

## Approfondimento
XML (eXtensible Markup Language) esiste dalla fine degli anni '90, un periodo in cui il web era ricco di testi e la necessità di un modo strutturato, ma flessibile per trasportare dati era cruciale. A causa della verbosità e complessità, XML ha perso terreno a favore di JSON. Tuttavia, XML è ancora prevalente, specialmente negli ambienti aziendali o nei protocolli come SOAP.

L'approccio di Elm all'XML è funzionale e type-safe. Utilizzare il pacchetto `elm/xml` significa abbracciare la filosofia Elm di esplicità e affidabilità. Quando si tratta di analisi, il pacchetto fornisce una gamma di decoder che componi per gestire la struttura XML.

Rispetto ad alternative come il DOMParser di JavaScript o ElementTree di Python, il metodo di Elm potrebbe sembrare più verboso ma garantisce sicurezza. Nessuna eccezione a runtime per campi mancanti o mismatch di tipi; se c'è qualcosa che non va, si ottiene un errore a tempo di compilazione.

Le funzioni di decodifica di `elm/xml` si basano sulla mappatura dei nodi XML ai tipi Elm. Si costruiscono decoder che riflettono la forma dei tuoi dati, garantendo che la tua app Elm gestisca l'XML con lo stesso rigore con cui gestisce le sue proprie strutture dati interne.

La generazione di XML è meno comune in Elm ma può essere realizzata con il corrispettivo di `elm/xml`, `Xml.Encode`.

## Vedi Anche
- Guida Elm su JSON che si applica anche alla mentalità XML: [https://guide.elm-lang.org/interop/json.html](https://guide.elm-lang.org/interop/json.html)
- Standard XML del W3C per una comprensione più profonda dell'XML stesso: [https://www.w3.org/XML/](https://www.w3.org/XML/)
