---
title:    "Elm: Verifica dell'esistenza di una directory"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler controllare se una directory esiste nel tuo programma Elm. Ad esempio, potresti voler sapere se puoi accedere a file specifici all'interno della directory, o se devi creare una nuova directory prima di poter salvare dei dati. Indipendentemente dalla ragione, imparare a controllare l'esistenza di una directory può essere molto utile per i tuoi progetti Elm.

## Come fare

Per controllare se una directory esiste in Elm, puoi utilizzare la funzione `Directory.exists` della libreria `elm/file`. Questa funzione accetta come argomento una stringa che rappresenta il percorso della directory e restituisce un valore `Task Bool`. Dovrai quindi utilizzare la funzione `Task.perform` per gestire il risultato della tua azione.

Ecco un esempio di codice che mostra come utilizzare la funzione `Directory.exists`:

```elm
import File exposing (Directory)
import Task

checkDirectoryExists : String -> Task.Tag
checkDirectoryExists path =
  Task.perform handleResult (Directory.exists path)

handleResult : Result x Bool -> Task x ()
handleResult result =
  case result of
    Ok exists ->
      if exists then
        -- la directory esiste
      else
        -- la directory non esiste
    Err error ->
      -- gestire eventuali errori
```

In questo esempio, stiamo utilizzando la funzione `checkDirectoryExists` per controllare se una directory specificata esiste o meno. Nota che la funzione `handleResult` viene chiamata solo quando la funzione `Task.perform` viene eseguita con successo. Se desideri gestire gli errori, dovrai aggiungere un'altra funzione `Task.perform` in cui gestire il caso in cui non è possibile controllare se la directory esiste.

## Approfondimento

Se sei interessato a saperne di più sui dettagli tecnici di come funziona la funzione `Directory.exists`, puoi dare un'occhiata alla sua implementazione all'interno della libreria `elm/file`. La libreria utilizza il pacchetto Node.js `fs-extra` per accedere al file system e controllare l'esistenza della directory.

## Vedi anche

- Documentazione ufficiale della funzione `elm/file Directory.exists`: https://package.elm-lang.org/packages/elm/file/latest/File-Directory#exists
- Tutorial su come utilizzare Elm per gestire e salvare file: https://guide.elm-lang.org/effects/file.html
- Tutorial su come utilizzare il pacchetto Node.js `fs-extra` in Elm: https://medium.com/@nickrtorrez/using-fs-extra-in-an-elm-app-c2f9ae3687e3