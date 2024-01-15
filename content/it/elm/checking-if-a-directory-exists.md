---
title:                "Verifica dell'esistenza di una directory"
html_title:           "Elm: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Se stai scrivendo un programma in Elm che deve operare con un insieme di directory, è importante essere sicuri che quelle directory esistano prima di accedervi. In questo articolo, vedremo come possiamo facilmente controllare se una directory esiste in Elm.

## Come Fare

Per controllare l'esistenza di una directory in Elm, possiamo utilizzare la funzione `Dir.exists` del modulo `FileSystem`. Questa funzione accetta come argomento il percorso della directory che vogliamo controllare e restituisce un valore di tipo `Task`.

```Elm
Dir.exists "path/to/directory"
    |> Task.map (\exists -> 
        if exists then
            "La directory esiste!"
        else
            "La directory non esiste :("
    )
    |> Task.perform (Debug.crash << toString)
```

Nell'esempio sopra, utilizziamo la funzione `Task.map` per elaborare il risultato della nostra chiamata alla funzione `Dir.exists`. Se il valore restituito è `True`, stampiamo un messaggio che indica l'esistenza della directory, altrimenti stampiamo un messaggio di errore. In seguito, utilizziamo la funzione `Task.perform` per eseguire la nostra operazione e gestire eventuali errori.

## Approfondimenti

La funzione `Dir.exists` si basa sulla API di JavaScript `fs.exists`, che controlla l'esistenza di un file o di una directory nel file system. Questo significa che, oltre a controllare l'esistenza di una directory, possiamo anche utilizzare questa funzione per verificare se un file esiste.

## Vedi Anche

- La documentazione ufficiale di `Dir.exists` in Elm: https://package.elm-lang.org/packages/elm/file/latest/FileSystem#exists
- La documentazione ufficiale di `fs.exists` in JavaScript: https://nodejs.org/api/fs.html#fs_fs_exists_path_callback