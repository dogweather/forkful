---
title:                "Elm: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché dovresti scaricare una pagina web?

Scaricare una pagina web può essere utile per molti motivi, come ad esempio analizzare il suo contenuto, estrarre informazioni specifiche o semplicemente avere una copia locale per consultazione offline.

## Come farlo in Elm?

Per scaricare una pagina web in Elm, puoi utilizzare il modulo `Http` all'interno della tua applicazione.

```elm
import Http

type Msg
    = DownloadResult (Http.Result Http.Error String)

downloadPage : Cmd Msg
downloadPage =
    Http.get "https://www.esempio.com"
        |> Http.send DownloadResult
```

Nel codice sopra, stiamo utilizzando la funzione `get` di `Http` per effettuare una richiesta GET all'URL specificato. Successivamente, utilizziamo la funzione `send` per inviare il risultato al nostro modello tramite il messaggio `DownloadResult`.

Il risultato della richiesta sarà un valore `Result`, che può essere gestito tramite il pattern matching. Ad esempio, possiamo stampare il contenuto della pagina nel nostro HTML utilizzando una condizione `case` nel nostro `view`:

```elm
case downloadResult of
    Ok pageContent ->
        text pageContent

    Err error ->
        text ("Errore durante il download: " ++ toString error)
```

Ecco un esempio di output:

```html
<!DOCTYPE html>
<html>
<head>
    <title>Esempio</title>
</head>
<body>
    <h1>Benvenuto su Elm!</h1>
</body>
</html>
```
## Approfondimento

La funzione `get` di `Http` accetta diversi parametri opzionali, come ad esempio un header personalizzato o un body della richiesta. Questi possono essere specificati utilizzando la sintassi di registrazione di Elm (record syntax) dopo l'URL:

```elm
Http.get "https://www.esempio.com" { headers = [ ( "Authorization", "Bearer <token>" ) ] , body = Http.emptyBody }
```

Inoltre, è possibile gestire diversi tipi di risposte nella funzione `send` utilizzando la funzione `map` di `Http`, che ci consente di mappare il risultato di una richiesta HTTP su una funzione che elabora un tipo specifico. Ad esempio, possiamo mappare il risultato su una funzione che restituisce una lista di stringhe invece di una singola stringa:

```elm
Http.get "https://www.esempio.com" { headers = [ ( "Authorization", "Bearer <token>" ) ] , body = Http.emptyBody }
    |> Http.map (\result -> case result of
        Ok pageContent ->
            String.lines pageContent

        Err error ->
            ["Errore durante il download: " ++ toString error]
    )
```

## Vedi anche

- [Documentazione di Elm - Modulo Http](https://package.elm-lang.org/packages/elm/http/latest/)
- [Esempio completo su Ellie - Scaricare una pagina web in Elm](https://ellie-app.com/6HfwNjhV5Fta1)
- [Tutorial Elm - Effettuare richieste HTTP](https://guide.elm-lang.org/effects/http.html)