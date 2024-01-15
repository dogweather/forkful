---
title:                "Scaricare una pagina web"
html_title:           "Javascript: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché scaricare una pagina web

Scaricare una pagina web può essere utile per vari motivi, come ad esempio per analizzare il codice sorgente o per archiviare una pagina importante.

## Come fare

È possibile scaricare una pagina web utilizzando la funzione `fetch()` di Javascript. Questa funzione richiede l'URL della pagina che si desidera scaricare come parametro e restituisce una Promise. Utilizzando il metodo `.then()`, possiamo accedere al contenuto della pagina e utilizzarlo per i nostri scopi.

Un esempio di codice per scaricare una pagina web e stampare il suo contenuto in console:

```Javascript
fetch("https://www.example.com")
  .then(response => response.text())
  .then(data => console.log(data));
```

Output:

```
<!DOCTYPE html>
<html>
  <head>
    <title>Example Domain</title>
    <meta charset="utf-8" />
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <style type="text/css">
      body {
        background-color: #f0f0f2;
        margin: 0;
        padding: 0;
        font-family: -apple-system, system-ui, BlinkMacSystemFont, "Segoe UI",
          "Open Sans", "Helvetica Neue", sans-serif;
      }
      div {
        width: 600px;
        margin: 5em auto;
        padding: 2em;
        background-color: #fdfdff;
        border-radius: 0.5em;
        box-shadow: 2px 3px 7px 2px rgba(0, 0, 0, 0.02);
      }
      a:link,
      a:visited {
        color: #38488f;
        text-decoration: none;
      }
      @media (max-width: 700px) {
        div {
          margin: 0 auto;
            width: auto;
          }
      }
    </style>
  </head>

  <body>
    <div>
      <h1>Example Domain</h1>
      <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
      <p><a href="https://www.iana.org/domains/example">More information...</a></p>
    </div>
  </body>
</html>
```

## Approfondimento

La funzione `fetch()` è molto potente e permette di specificare varie opzioni, come ad esempio il metodo di richiesta, l'header e il body della richiesta. Inoltre, restituisce una risposta di tipo `Response` che ha diversi metodi per accedere ai diversi aspetti della risposta, come ad esempio il codice di stato o gli header.

Per maggiori informazioni su come utilizzare la funzione `fetch()`, si consiglia di consultare la documentazione ufficiale di Javascript: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API.

## Vedi anche

- Documentazione ufficiale di Javascript sulla funzione `fetch()`: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Esempi di codice per il download di pagine web con Javascript: https://www.w3schools.com/js/js_ajax_intro.asp