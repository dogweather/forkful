---
title:                "Elm: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Sai mai quando hai bisogno di sapere se una cartella esiste o meno durante la programmazione? Forse stai cercando di leggere o scrivere file in una cartella specifica, o forse stai cercando di creare una nuova cartella. In ogni caso, è importante controllare se una cartella esiste prima di tentare qualsiasi operazione su di essa per evitare errori nel tuo programma.

## Come fare

Per verificare se una cartella esiste in Elm, puoi utilizzare la funzione `directoryExists` del pacchetto `elm/file`. Questa funzione richiede il path della cartella che vuoi controllare e restituisce un `Task` che restituirà un `Bool` che rappresenta se la cartella esiste o meno.

```
import File
import Task

folderPath : String
folderPath = "./myfolder" -- sostituisci con il path della tua cartella

task : Task.error File.Error Bool
task = File.directoryExists folderPath

main : Task.error File.Error ()
main =
    Task.perform
        (\err -> -- gestione dell'errore)
        (\exists -> -- operazioni sulla cartella)
        task
```

Se la cartella esiste, il valore `exists` sarà `True`, altrimenti sarà `False`.

## Approfondimento

È importante notare che la funzione `directoryExists` non effettua effettivamente un controllo diretto sulla cartella, ma crea un `Task` che, una volta eseguito, verificherà l'esistenza della cartella. Ciò è utile perché ti permette di controllare l'esistenza della cartella in un momento successivo, quando potrebbe essere necessario.

Inoltre, il pacchetto `elm/file` offre anche altre funzioni utili per lavorare con le directory, come ad esempio `createDirectory` per creare una nuova cartella, `getDirectoryContents` per ottenere una lista dei file all'interno di una cartella, e `removeDirectory` per rimuovere una cartella.

## Vedi anche

- [Documentazione del pacchetto `elm/file`](https://package.elm-lang.org/packages/elm/file/latest/)
- [Esempi di utilizzo di `elm/file`](https://github.com/elm/file/tree/master/examples)