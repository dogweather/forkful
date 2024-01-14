---
title:    "Elm: Creazione di un file temporaneo"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Perché creare file temporanei con Elm

Creare file temporanei può essere utile per svariati motivi, come ad esempio la gestione di dati temporanei o la creazione di file di backup durante la modifica di un progetto. Inoltre, con Elm è possibile creare file temporanei in modo semplice e sicuro grazie alla sua natura funzionale e al controllo degli effetti collaterali.

## Come creare un file temporaneo con Elm

Per creare un file temporaneo con Elm, è necessario utilizzare la libreria `elm/file` che permette di gestire l'accesso ai file di sistema. Innanzitutto, è necessario definire un modello che rappresenti il file temporaneo, ad esempio:

```elm
type alias TempFile =
    { name : String
    , content : String
    }
```

Successivamente, è possibile utilizzare la funzione `tempFile` per creare il file temporaneo:

```elm
tempFile : String -> String -> Cmd Msg
tempFile fileName tempData =
    Cmd.map FileCreated (File.tempFile fileName tempData)
```

Infine, è possibile gestire la ricezione del file temporaneo tramite il `Msg` corrispondente:

```elm
type Msg
    = FileCreated (Maybe File.Result)
```

## Approfondimenti sulla creazione di file temporanei

Per ottenere maggior controllo sulla creazione di file temporanei con Elm, è possibile utilizzare la funzione `tempFileWith` che permette di specificare alcune opzioni aggiuntive, come il percorso in cui salvare il file temporaneo o le autorizzazioni di lettura e scrittura.

Inoltre, è possibile utilizzare la libreria `elm/random` per generare nomi casuali di file, così da evitare di sovrascrivere eventuali file esistenti.

# Vedi anche
- [Documentazione ufficiale della libreria elm/file](https://package.elm-lang.org/packages/elm/file/latest/)
- [Esempio di creazione di file temporanei con Elm](https://gist.github.com/evancz/2bfcba262f318fbd5895ab0f0a4d5120)
- [Libreria elm/random per la generazione di nomi casuali](https://package.elm-lang.org/packages/elm/random/latest/)