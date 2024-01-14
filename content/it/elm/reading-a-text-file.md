---
title:                "Elm: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

Perché: Le operazioni di lettura dei file di testo sono fondamentali nella programmazione Elm. Imparare come farlo ti permetterà di manipolare dati e creare applicazioni più avanzate.
 
Come Fare: Ecco una semplice guida per leggere un file di testo utilizzando la funzione `Text.fromFile` in Elm.

```Elm
import Text

-- Creiamo un file di testo chiamato "mio_file.txt" con alcuni dati al suo interno, ad esempio: "1, 2, 3, 4, 5"

main =
    Text.fromFile "mio_file.txt"
        |> Task.perform readFileResult

readFileResult : Result String String -> Html msg
readFileResult result =
    case result of
        Ok textFromFile ->
            -- stampa il contenuto del file nel browser
            div [] [ text textFromFile ]

        Err error ->
            -- gestisci eventuali errori
            text ("Errore nella lettura del file: " ++ error)
```

Esempio di output in un browser:

`1, 2, 3, 4, 5`

Deep Dive: Ora che hai un'idea di come leggere un file di testo in Elm, puoi approfondire ulteriormente il concetto esplorando la libreria di gestione dei file `elm/file`. Questa libreria fornisce funzioni più avanzate per la gestione dei file, come la scrittura, la cancellazione e il controllo dei permessi. Inoltre, puoi esplorare l'uso di decoder per leggere e convertire dati strutturati all'interno di un file di testo.

See Also:
- [Elm - Gestione dei File](https://guide.elm-lang.org/effects/file.html)
- [Documentazione Elm - Text.fromFile](https://package.elm-lang.org/packages/elm/file/latest/Text#fromFile)
- [Elm - Decoding Data](https://guide.elm-lang.org/effects/json.html)