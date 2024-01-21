---
title:                "Inviare una richiesta http"
date:                  2024-01-20T17:59:11.514084-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Inviamo richieste HTTP per comunicare con server web e scambiare dati. Lo facciamo perché è alla base di quasi ogni interazione tra client e Internet: da richiedere una pagina web fino all'interazione con servizi API.

## How to: (Come fare:)
```C
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;

    // Inizializza libCURL
    curl_global_init(CURL_GLOBAL_DEFAULT);
    
    // Crea l'oggetto CURL
    curl = curl_easy_init();
    if(curl) {
        // Imposta l'URL da cui effettuare la richiesta HTTP
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        
        // Esegui la richiesta HTTP
        res = curl_easy_perform(curl);
        
        // Verifica che non ci siano errori
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        }

        // Libera le risorse di CURL
        curl_easy_cleanup(curl);
    }

    // Pulisci globalmente libCURL
    curl_global_cleanup();

    return 0;
}
```
Output:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive (Approfondimento)
Invio di richieste HTTP non è una funzione nativa del linguaggio C. Bisogna usare librerie esterne come libCURL, che è stata creata nel 1997 e da allora è divenuta il modo standard per le richieste HTTP in C. Un'alternativa è scrivere codice a basso livello direttamente con le socket API di POSIX, ma è più complesso. Implementare la propria soluzione richiede conoscenza di protocolli di rete e gestione delle connessioni, mentre libCURL nasconde questa complessità.

## See Also (Vedi Anche)
- [libcurl - the multiprotocol file transfer library](https://curl.haxx.se/libcurl/)
- [HTTP Made Really Easy - A Practical Guide to Writing Clients and Servers](http://www.jmarshall.com/easy/http/)
- [POSIX Sockets - Beej's Guide to Network Programming](https://beej.us/guide/bgnet/)