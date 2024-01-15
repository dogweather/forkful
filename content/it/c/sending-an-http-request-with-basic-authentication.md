---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "C: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perchè

La richiesta HTTP con autenticazione di base è importante quando si vuole accedere ad un servizio web protetto da un sistema di login. Questa autenticazione garantisce una maggiore sicurezza nei dati scambiati tra il cliente e il server.

## Come Fare

Per inviare una richiesta HTTP con autenticazione di base in C, è necessario seguire i seguenti passaggi:

1. Includere le librerie necessarie utilizzando l'istruzione ```#include <curl/curl.h>```
2. Inizializzare la struttura di autenticazione di base tramite la funzione ```curl_easy_setopt()```
3. Impostare l'autenticazione di base con le credenziali corrette
4. Eseguire la richiesta HTTP con la funzione ```curl_easy_perform()```

Un esempio di codice completo potrebbe essere:

```
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
    CURL *curl;
    CURLcode res;
    
    curl = curl_easy_init();
    
    if (curl) {
        // Inizializza l'autenticazione di base
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        
        // Imposta le credenziali
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");
        
        // Esegue la richiesta HTTP
        curl_easy_perform(curl);
        
        // Rilascia la risorsa CURL
        curl_easy_cleanup(curl);
        
        // Gestisce eventuali errori
        if (res != CURLE_OK) {
            fprintf(stderr, "Errore nella richiesta: %s\n",
                    curl_easy_strerror(res));
            return 1;
        }
    }
    
    return 0;
}
```

Ecco un esempio di output che potresti ottenere:

```
HTTP/1.1 200 OK
Date: Tue, 01 Oct 2019 12:00:00 GMT
Server: Apache/2.4.38 (Unix)
Last-Modified: Thu, 27 Jun 2019 10:00:00 GMT
ETag: "1234"
Accept-Ranges: bytes
Content-Length: 123
Content-Type: text/html

<html>
    <head>
        <title>Ciao Mondo!</title>
    </head>
    <body>
        <h1>Ciao Mondo!</h1>
    </body>
</html>
```

## Approfondimenti

L'autenticazione di base è soltanto uno dei tanti metodi di autenticazione supportati dalle specifiche HTTP. Altri metodi includono l'autenticazione con token, l'autenticazione digest e l'autenticazione OAuth. Inoltre, è possibile anche implementare un proprio sistema di autenticazione personalizzato.

L'uso dell'autenticazione di base è ancora molto comune nei servizi web, ma non è considerato un metodo sicuro perché i dati di login viaggiano in chiaro. Si consiglia di utilizzare metodi di autenticazione più sicuri nei casi in cui sia necessaria una maggiore protezione dei dati.

## Vedi Anche

- [Specifiche HTTP](https://tools.ietf.org/html/rfc2616)
- [Documentazione CURL](https://curl.haxx.se/libcurl/c)
- [Autenticazione di base su Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)