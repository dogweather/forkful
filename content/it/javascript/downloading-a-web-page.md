---
title:                "Javascript: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Scaricare una pagina web è una delle attività più comuni per un programmatore Javascript. Con questo semplice processo, è possibile acquisire dati in modo efficiente e utilizzarli per creare applicazioni web dinamiche.

## Come fare

Per scaricare una pagina web, possiamo utilizzare la funzione ````fetch()```` disponibile nativamente in Javascript. Possiamo passare l'URL della pagina come argomento e la funzione restituirà una promessa con la risposta nella forma di un oggetto ````Response````.

Per esempio:

````Javascript
fetch(URL).then(response => response.text())
  .then(data => console.log(data));
````

Questo codice ci permette di scaricare il contenuto testuale di una pagina web e di visualizzarlo sulla console. Possiamo anche utilizzare il metodo ````json()```` al posto di ````text()````per ottenere i dati in formato JSON.

## Approfondimento

Mentre la funzione ````fetch()```` è semplice e facile da usare, ci sono diversi aspetti da considerare quando si scarica una pagina web. Ad esempio, è importante gestire anche gli errori che potrebbero verificarsi durante il processo di download.

Inoltre, dobbiamo prestare attenzione alle politiche di sicurezza del browser, che possono impedire il download di una pagina da un dominio diverso da quello in cui il codice è eseguito. Per risolvere questo problema, possiamo utilizzare il protocollo di sicurezza CORS (Cross-Origin Resource Sharing).

Inoltre, sappiamo che le pagine web possono contenere risorse esterne, come immagini o file CSS. Per scaricare anche queste risorse, è possibile utilizzare una libreria come ````puppeteer```` che ci permette di emulare un browser e di accedere a tutti gli elementi di una pagina.

Oltre a ciò, dobbiamo anche considerare il tempo di risposta del server e la possibilità di dover gestire il caching della pagina nel nostro codice.

## Guarda anche

- [Documentazione ufficiale di Fetch API](https://developer.mozilla.org/it/docs/Web/API/Fetch_API)
- [Esempi pratici di utilizzo di Fetch API](https://flaviocopes.com/fetch-api/)
- [Guida a CORS su Mozilla Developer Network](https://developer.mozilla.org/it/docs/Web/HTTP/CORS)