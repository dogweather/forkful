---
title:                "Lettura di un file di testo."
html_title:           "Elm: Lettura di un file di testo."
simple_title:         "Lettura di un file di testo."
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Se sei nuovo alla programmazione in Elm, potresti chiederti perché dovresti leggere un file di testo. La risposta è semplice: molti programmi utilizzano file di testo come mezzo per memorizzare e gestire dati, quindi è importante saperne leggere il contenuto per poter creare programmi efficaci.

## Come fare

Per leggere un file di testo in Elm, è necessario utilizzare la funzione `File.toText` del modulo `File`. Questa funzione accetta come parametro il percorso del file e restituisce un valore di tipo `Task Text Error`, che rappresenta l'operazione di lettura del file. Per ottenere il contenuto effettivo del file, è necessario eseguire la `Task` utilizzando la funzione `Task.perform`. Vediamo un esempio pratico:

```Elm
import File exposing (toText)
import Task

file : String
file = "test.txt" -- sostituire con il percorso del tuo file

result : Task Text Error
result = toText file

main : Program Never
main = 
  Task.perform (\text -> 
    case text of 
      Ok content -> 
        -- fa qualcosa con il contenuto del file 
      Err err -> 
        -- gestisci l'eventuale errore di lettura del file 
  ) result

``` 

In questo esempio, la funzione `toText` restituisce un `Task Text Error` che viene eseguito da `Task.perform` e passato a una funzione che gestisce il suo output. È importante gestire l'eventuale errore (`Err err`) poiché il file potrebbe non esistere o non essere leggibile.

## Approfondimento

Leggere un file di testo in Elm è possibile grazie al supporto per le `Task`, che rappresentano operazioni asincrone che possono essere eseguite in background. Inoltre, Elm fornisce funzioni per gestire facilmente gli errori delle `Task` attraverso costrutti come `Result` e `Maybe`.

## Vedi anche

- Documentazione ufficiale di Elm su `File` modulo: https://package.elm-lang.org/packages/elm/file/latest/
- Esempi di operazioni con file in Elm: https://github.com/elm/projects/tree/master/hello-world/hello, file