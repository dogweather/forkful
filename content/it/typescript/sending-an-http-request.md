---
title:                "Invio di una richiesta http"
html_title:           "TypeScript: Invio di una richiesta http"
simple_title:         "Invio di una richiesta http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

Perché utilizzare TypeScript per inviare una richiesta HTTP?

Se sei un programmatore che utilizza TypeScript, inviare una richiesta HTTP è spesso una parte essenziale del processo di sviluppo. Ciò ti consente di comunicare con un server e ottenere dati o eseguire azioni in base alle tue esigenze.

## Come procedere

Per inviare una richiesta HTTP utilizzando TypeScript, è necessario prima importare il modulo `http` e creare un'istanza della classe `HttpClient` come mostrato di seguito:

```TypeScript
import { HttpClient } from 'http';

const client = new HttpClient();
```

Una volta creato il client, puoi utilizzare il metodo `get` o `post` per inviare una richiesta a un server. Ad esempio, se voglio ottenere una lista di utenti dal server, posso utilizzare il metodo `get` nel seguente modo:

```TypeScript
client.get('https://www.example.com/users')
  .then(response => {
    console.log(response.data); // output: [{id: 1, name: "John"}, {id: 2, name: "Jane"}]
  })
  .catch(error => {
    console.log(error.message); // output: Error: Request failed
  });
```

Puoi anche passare dei parametri nella richiesta, come ad esempio un ID per ottenere i dettagli di un utente specifico:

```TypeScript
client.get(`https://www.example.com/users/${userId}`)
  .then(response => {
    console.log(response.data); // output: {id: 1, name: "John", email: "john@example.com"}
  })
  .catch(error => {
    console.log(error.message); // output: Error: Request failed
  });
```

Se invece devi inviare una richiesta `post`, puoi utilizzare il metodo `post` e fornire i dati da inviare nel corpo della richiesta:

```TypeScript
const newUser = {
  name: "Emily",
  email: "emily@example.com"
}

client.post('https://www.example.com/users', newUser)
  .then(response => {
    console.log(response.data); // output: User created successfully
  })
  .catch(error => {
    console.log(error.message); // output: Error: Request failed
  });
```

## Approfondimento

Inoltre, puoi specificare opzioni aggiuntive come gli header della richiesta o i parametri di query usando un oggetto di opzioni come secondo parametro nei metodi `get` e `post`. Inoltre, puoi anche modificare manualmente l'oggetto `httpClient` per personalizzare comportamenti come il timeout della richiesta.

È importante gestire correttamente gli errori nelle chiamate HTTP, in modo da poter reagire in modo appropriato se la richiesta non ha successo. Inoltre, devi anche gestire le risposte in modo corretto e assicurarti di utilizzare i tipi corretti quando si definiscono i dati ricevuti.

## Vedi anche

- [Documentazione TypeScript su HttpClient](https://www.typescriptlang.org/docs/handbook/using-external-libraries.html#using-an-external-module-without-type-information)
- [Esempi di codice TypeScript per l'uso di HttpClient](https://github.com/Microsoft/TypeScript/blob/master/samples/3rd/d3/d3-fetch-test.ts)