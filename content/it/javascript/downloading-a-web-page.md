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

## Cosa e Perchè?

Scaricare una pagina web è il processo di acquisizione di un file HTML da un server remoto e visualizzarlo sul proprio dispositivo locale. I programmatori spesso eseguono questa operazione per ottenere dati da un sito web o per automatizzare determinate attività, come il web scraping.

## Come fare:

```Javascript
// Esempio di download di una pagina web
const fetch = require("node-fetch"); // Importa il modulo "node-fetch" 

fetch("https://www.google.com") // Esegue una richiesta GET all'URL specificato
  .then((response) => response.text()) // Converte la risposta in testo
  .then((data) => console.log(data)); // Stampa il contenuto della pagina web scaricata
```

Output: 
```html
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8" />
  <meta http-equiv="Content-Security-Policy" content="upgrade-insecure-requests" />
  <meta name="viewport" content="width=device-width,initial-scale=1" />
  <title>Google</title>
  <script nonce="lNkgbQ2SYFCfcDyJhiWngg==">(function(){window.google={kEI:'...',kEXPI:'...'};

// Resto del codice HTML della pagina scaricata
```

## Approfondimento:

- Contesto storico: Nel passato, i programmatori scaricavano le pagine web utilizzando linguaggi di programmazione come Perl e Python. Con l'avvento di Javascript come linguaggio di scripting lato client, scaricare una pagina web è diventato più semplice e veloce.
- Alternative: Oltre all'uso del modulo "node-fetch", esistono anche altri modi per scaricare una pagina web in Javascript, come l'utilizzo del framework "axios" o delle API del browser.
- Dettagli di implementazione: Il modulo "node-fetch" è basato sulla specifica Fetch API, che fornisce un'interfaccia per recuperare risorse da un server remoto utilizzando promesse. È possibile specificare diverse opzioni nella richiesta, come i parametri della richiesta e gli header.

## Vedi anche:

- Documentazione del modulo "node-fetch": https://www.npmjs.com/package/node-fetch
- Fetch API su MDN: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Documentazione di axios: https://axios-http.com/