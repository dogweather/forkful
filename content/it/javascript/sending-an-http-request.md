---
title:                "Invio di una richiesta http"
html_title:           "Javascript: Invio di una richiesta http"
simple_title:         "Invio di una richiesta http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

L'invio di una richiesta HTTP è un'operazione comune nella programmazione web. È il modo in cui i programmatori fanno comunicare il loro codice con altri server per ottenere dati o eseguire azioni. Questo può essere fatto tramite browser o utilizzando librerie di codice come Axios o Fetch.

## Come fare:

### Esempio 1: Utilizzo del metodo GET per ottenere dati da un server

```Javascript
fetch('https://api.example.com/posts')
.then(response => response.json())
.then(data => console.log(data));
```

Output: Un array di oggetti contenenti post dal server richiesto.

### Esempio 2: Utilizzo del metodo POST per inviare dati a un server

```Javascript
const data = { title: 'Nuovo Post', body: 'Contenuto del post' };
const options = {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json'
  },
  body: JSON.stringify(data)
};

fetch('https://api.example.com/posts', options)
.then(response => response.json())
.then(data => console.log(data));
```

Output: L'oggetto appena creato, con un ID univoco assegnato dal server.

## Approfondimento:

Questo è solo uno degli innumerevoli modi in cui è possibile inviare una richiesta HTTP in Javascript. Oltre alla libreria Fetch, esistono anche altre opzioni come jQuery o Axios. Inoltre, è importante tenere conto della sicurezza quando si inviano dati sensibili tramite richieste HTTP.

## Vedi anche:

- [Documentazione ufficiale Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Documentazione ufficiale Axios](https://axios-http.com/docs/intro)