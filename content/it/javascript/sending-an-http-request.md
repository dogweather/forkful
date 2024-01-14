---
title:                "Javascript: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

Perché inviare una richiesta HTTP è importante nel mondo della programmazione JavaScript?

Il motivo principale per cui si invia una richiesta HTTP è per ottenere dati da un server esterno. Questo è particolarmente utile quando si costruiscono applicazioni web che richiedono un'interazione costante con dati esterni, come ad esempio una chat o un feed di notizie.

Come farlo:

Per inviare una richiesta HTTP in JavaScript, è necessario utilizzare il metodo `XMLHttpRequest` (o `fetch` se si utilizza una versione più recente di JavaScript). Di seguito è riportato un esempio di codice che illustra come inviare una richiesta HTTP utilizzando `XMLHttpRequest`:

```Javascript
let request = new XMLHttpRequest();
request.open("GET", "https://api.example.com/data");
request.send();
```

Questo codice crea un nuovo oggetto `XMLHttpRequest` e lo configura per inviare una richiesta GET all'URL specificato. Una volta che la richiesta viene inviata, è possibile utilizzare la seguente funzione per gestire la risposta:

```Javascript
request.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    console.log(this.responseText);
  }
};
```

Questa funzione di callback verrà eseguita quando la risposta viene ricevuta dal server e avrà uno stato di `4` (completato) e uno stato di `200` (risposta OK). In questo esempio, stiamo semplicemente stampando il corpo della risposta a console, ma è possibile utilizzare i dati ricevuti per popolare la pagina web o eseguire altre operazioni.

Deep Dive:

Oltre alla semplice richiesta GET mostrata sopra, è possibile inviare una varietà di altri tipi di richieste HTTP utilizzando `XMLHttpRequest` o` fetch`. Ad esempio, è possibile inviare una richiesta POST per inviare dati al server, una richiesta PUT per aggiornare dati esistenti, o una richiesta DELETE per eliminare dati.

È anche possibile includere parametri aggiuntivi nella richiesta, come intestazioni personalizzate o dati del corpo. Inoltre, è possibile utilizzare `HTTP post` per eseguire richieste asincrone, che permettono di gestire più richieste contemporaneamente in modo più efficiente.

Inoltre, esistono molte librerie JavaScript che semplificano il processo di invio di richieste HTTP, come Axios e SuperAgent. Queste librerie offrono funzionalità aggiuntive come la gestione degli errori e la conversione automatica dei dati in formato JSON.

See Also:

- [Una guida completa all'invio di richieste HTTP in JavaScript](https://www.digitalocean.com/community/tutorials/js-invoke-http)
- [Documentazione ufficiale di XMLHTTPRequest](https://developer.mozilla.org/it/docs/Web/API/XMLHttpRequest)
- [Librerie JavaScript popolari per inviare richieste HTTP](https://blog.bitsrc.io/11-reactjs-component-libraries-you-should-have-in-your-tool-box-f538a89066b3)