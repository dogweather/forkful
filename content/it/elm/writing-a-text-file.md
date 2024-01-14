---
title:                "Elm: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Sei pronto ad entrare nel mondo della programmazione funzionale e ad imparare Elm? Scopri come scrivere un file di testo con questo linguaggio semplice e potente!

## Come fare

Per cominciare a scrivere un file di testo in Elm, utilizziamo la libreria `elm-io` che ci permette di comunicare con il filesystem. Innanzitutto, importiamo la libreria e creiamo una funzione che prenderà in input il contenuto del file che vogliamo creare.

```Elm
import File
import Task exposing (succeed)
import Time exposing (Posix)

writeFile : String -> Task.Task String ()
writeFile content =
    Task.map (\_ -> ()) <| File.write "mio_file.txt" content
```

Nella nostra funzione, utilizziamo la funzione `write` di `File` che prende come primo parametro il nome del file e come secondo parametro il suo contenuto. In questo caso, il contenuto sarà semplicemente una stringa, ma si può adattare il codice per scrivere dati più complessi come JSON o CSV.

Una volta creato il file, possiamo chiamare la nostra funzione `writeFile` e passargli la stringa che vogliamo scrivere:

```Elm
main : Posix -> Task.Task String ()
main _ =
    writeFile "Questo è il contenuto del mio file"
```

Ora se eseguiamo la nostra funzione `main`, dovremmo ottenere un output che ci conferma che il file è stato creato con successo.

```bash
elm-io-mio_file.txt created successfully.
```

## Approfondimenti

Ora che abbiamo imparato come scrivere un file di testo in Elm, possiamo esplorare ulteriormente il vasto mondo di questo linguaggio funzionale. Ti consigliamo di dare un'occhiata alle seguenti risorse:

[Elm Italia (https://elmitalia.github.io/](https://elmitalia.github.io/) - Una community italiana di appassionati di Elm, con forum, tutorial e molte risorse utili per imparare.

[Elm Docs (https://guide.elm-lang.org/](https://guide.elm-lang.org/) - La guida ufficiale di Elm, con esempi e spiegazioni dettagliate su come utilizzare questo linguaggio.

See Also:

[Elm Italia](https://elmitalia.github.io/) - Una community italiana di appassionati di Elm.
[Elm Docs](https://guide.elm-lang.org/) - La guida ufficiale di Elm.