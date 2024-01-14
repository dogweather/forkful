---
title:    "Elm: Verifica dell'esistenza di una directory"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché
Controllare se una directory esiste può essere un'operazione importante quando si lavora con file e dati all'interno di un programma Elm. Essere in grado di verificare facilmente se una directory è presente o meno può aiutare a garantire la validità e l'integrità dei dati utilizzati.

## Come fare
Per verificare se una directory esiste in Elm, possiamo utilizzare la funzione `File.System.isDirectory` e passare come parametro il percorso della directory che vogliamo controllare. Di seguito un esempio di codice:

```Elm
import File.System exposing (Directory, isDirectory)

isExistingDirectory : Directory -> Task x Bool
isExistingDirectory directory =
   isDirectory directory
```

L'output di questa funzione sarà un `Task`, che ci permette di gestire in modo asincrono il risultato della verifica. Possiamo utilizzare la funzione `Task.andThen` per eseguire altre operazioni sul risultato, come ad esempio stampare un messaggio a schermo se la directory esiste oppure gestire eventuali errori.

```Elm
isExistingDirectory "cartella_mia" |> Task.andThen
   (\exists ->
       if exists then
           -- la directory esiste, si possono eseguire altre operazioni
           Task.succeed ()
       else
           -- la directory non esiste, possiamo gestire l'errore
           Task.fail "La directory non esiste"
   )
```

## Approfondimento
Potrebbe essere utile sapere che il tipo `Task` in Elm viene utilizzato per gestire operazioni asincrone, come l'accesso a file e directory. L'utilizzo di questo tipo ci permette di gestire in modo sicuro eventuali errori e di scrivere codice più robusto e affidabile. Inoltre, la funzione `File.System.isDirectory` fa parte del pacchetto `elm/file` che contiene diverse utili funzioni per lavorare con file e directory.

## Vedi anche
- [Documentazione ufficiale di Elm su File System](https://package.elm-lang.org/packages/elm/file/latest/)
- [Esempi di utilizzo di Task in Elm](https://guide.elm-lang.org/error_handling/tasks.html)
- [Gestione degli errori in Elm](https://guide.elm-lang.org/error_handling/)