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

##Che cos'è e perché fare una richiesta HTTP?

In poche parole, una richiesta HTTP (Hypertext Transfer Protocol) è un modo per comunicare con un server e ottenere dati o eseguire un'azione. I programmatori spesso inviano richieste HTTP per ottenere informazioni per le loro applicazioni o per creare interazioni con altri servizi online.

##Come fare una richiesta HTTP in TypeScript

Di seguito è riportato un esempio di come fare una richiesta HTTP utilizzando TypeScript. Nota che è necessario installare il pacchetto `axios` per utilizzare il modulo `axios` nella nostra applicazione.

```TypeScript
import axios from "axios";

// Eseguire una richiesta GET per ottenere dati da un URL
axios.get("https://jsonplaceholder.typicode.com/todos/1")
    .then(response => {
        console.log(response.data);
    })
    .catch(error => {
        console.log(error);
    });
```

Output:

```
{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

Allo stesso modo, è anche possibile eseguire richieste POST, PUT, DELETE e altre utilizzando il metodo corrispondente di `axios`.

##Approfondimento

Le richieste HTTP sono state introdotte per la prima volta nel 1991 come parte dell'HTTP protocollo 0.9. Da allora, l'HTTP è diventato uno dei protocolli più importanti per la comunicazione sul web. Anche se TypeScript ha un supporto integrato per le richieste HTTP, c'è anche il pacchetto `fetch` che può essere utilizzato per fare richieste HTTP.

##Vedi anche

Per ulteriori informazioni sul modulo `axios`, i seguenti link possono essere utili:

- Documentazione ufficiale di [axios](https://github.com/axios/axios)
- Tutorial su [come utilizzare axios con TypeScript](https://www.digitalocean.com/community/tutorials/react-typescript-axios)