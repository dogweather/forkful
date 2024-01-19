---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Inviare una richiesta HTTP è essenzalmente il processo di chiedere informazioni a un server web. I programmatori lo fanno per interagire con API web, recuperare o inviare dati.

## Come fare:

Ecco un esempio con Javascript utilizzando il metodo `fetch()`:

```Javascript
fetch('https://api.example.com/data', {
  method: 'GET',
})
.then(response => response.json())
.then(data => console.log(data))
.catch((error) => {
  console.error('Errore:', error);
});
```

Output previsto (sample):

```Javascript
{
  "key1": "value1",
  "key2": "value2"
}
```

## Immersione Profonda

Iniziando con la versione 0.10.0, Node.js prevedeva il modulo 'http' per le richieste HTTP. Nonostante ciò, con l'avvento di nuove nuove librerie come `fetch` e `axios`, ora è più comune e semplice utilizzare queste alternative più moderne.

`fetch` è nativo nei browser più recenti, e torna una Promise. Se invece preferisci un approccio basato su async/await, `axios` potrebbe essere la scelta giusta per te.

Ecco un breve esempio su come utilizzare `axios`:

```Javascript
const axios = require('axios');

axios.get('https://api.example.com/data')
.then(function (response) {
  console.log(response.data);
})
.catch(function (error) {
  console.error(error);
});
```

## Vedi Anche

[Documentazione ufficiale Fetch API su MDN](https://developer.mozilla.org/it/docs/Web/API/Fetch_API)

[Documentazione ufficiale Axios su GitHub](https://github.com/axios/axios)

[Github Fetch API Polyfill](https://github.com/github/fetch) per supportare browser più vecchi.