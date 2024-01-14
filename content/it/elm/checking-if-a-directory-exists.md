---
title:                "Elm: Verificare se una directory esiste"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

La verifica dell'esistenza di una directory è uno dei compiti fondamentali nella programmazione. Sapere se una determinata directory esiste o meno è importante per garantire che il programma funzioni correttamente e per gestire gli errori in modo efficiente.

## Come fare

Per verificare se una directory esiste in Elm, è possibile utilizzare la funzione `elm/file` di Elm. Ecco un esempio di codice che illustra come farlo:

```Elm
import File
import Task exposing (Task)

verificaDirectory : Task File.Error Bool
verificaDirectory =
  File.exists "percorso/alla/directory"
```

In questo esempio, stiamo importando il modulo `File` e la funzione `exists` che ci permette di verificare l'esistenza di una directory. Passiamo come argomento il percorso alla directory che vogliamo verificare e la funzione ci restituirà un `Task` che rappresenta il risultato della nostra verifica.

Per eseguire il `Task` e ottenere il risultato effettivo, possiamo utilizzare la funzione `Task.perform` e gestire il risultato in modo appropriato. Ad esempio, possiamo stampare il risultato a console utilizzando `Debug.log`:

```Elm
Task.perform
  (\exists -> Debug.log "La directory esiste? " (toString exists))
  (\error -> Debug.log "Errore nella verifica della directory" error)
  verificaDirectory
```

Questo codice stamperà `True` o `False` a seconda che la directory esista o meno.

## Approfondimenti

La funzione `File.exists` ritornerà un `Task File.Error Bool`, che può essere interpretato come una `Promise` in JavaScript. Se non si è familiari con questa struttura, è consigliato approfondire le proprie conoscenze prima di utilizzarla in modo efficace.

## Vedi anche

- Documentazione ufficiale di `elm/file`: https://package.elm-lang.org/packages/elm/file/latest/
- Un tutorial su come utilizzare `Task` in Elm: https://dennisreimann.de/articles/elm-task.html
- Una guida di riferimento su come gestire gli errori in Elm: https://elmprogramming.com/handling-errors-elm.html