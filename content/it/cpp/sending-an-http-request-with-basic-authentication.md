---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "C++: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché
Sending an HTTP request with basic authentication is necessary when accessing web resources that require a user to authenticate themselves. This allows for secure access to the resource, ensuring that only authorized users can access it.

## Come fare
Per inviare una richiesta HTTP con autenticazione di base in C++, è necessario seguire i seguenti passaggi:

1. Includere la libreria `curl/curl.h` nel tuo programma.
2. Creare un'istanza della struttura `CURL`.
  ```C++
  CURL *curl = curl_easy_init();
  ```
3. Definire l'URL del sito web a cui desideri inviare la richiesta.
  ```C++
  curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/api/login");
  ```
4. Specificare che desideri utilizzare l'autenticazione di base nella tua richiesta.
  ```C++
  curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
  ```
5. Impostare il nome utente e la password per l'autenticazione.
  ```C++
  curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");
  ```
6. Eseguire la richiesta HTTP utilizzando la funzione `curl_easy_perform`.
  ```C++
  CURLcode res = curl_easy_perform(curl);
  ```
7. Eseguire il cleanup della struttura `CURL`.
  ```C++
  curl_easy_cleanup(curl);
  ```
8. Verificare il codice di risposta della richiesta per determinare se l'operazione è stata eseguita con successo.
  ```C++
  if (res != CURLE_OK) {
    // In caso di errore, visualizza un messaggio di errore
    fprintf(stderr, "curl_easy_perform() failed: %s\n",
            curl_easy_strerror(res));
  }
  ```

Esempio di output di una richiesta HTTP con autenticazione di base:

```
< HTTP/1.1 200 OK
< Content-Type: application/json
< Content-Length: 116
<
{"username": "john_doe", "email": "john_doe@example.com", "loggedIn": true}
```

## Deep Dive
Quando si utilizza l'autenticazione di base in una richiesta HTTP, il nome utente e la password vengono codificati utilizzando l'algoritmo di codifica Base64. Questo significa che le credenziali non sono crittografate e possono essere facilmente decodificate da chiunque conosca l'algoritmo di codifica. Per questo motivo, l'utilizzo della autenticazione di base non è consigliato per scopi di sicurezza e dovrebbe essere utilizzata solo per scopi di test o in ambienti interni.

Inoltre, l'autenticazione di base viene spesso utilizzata in combinazione con una connessione HTTPS per fornire un livello aggiuntivo di sicurezza.

## Vedi anche
- [Tutorial: Utilizzo di libcurl in C++](https://curl.se/libcurl/c/example.html)
- [Documentazione di libcurl](https://curl.se/libcurl/)
- [Specifiche HTTP 1.1 sulla autenticazione di base](https://tools.ietf.org/html/rfc2617)