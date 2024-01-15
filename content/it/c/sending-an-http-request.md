---
title:                "Inviare una richiesta http"
html_title:           "C: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

Mandare una richiesta HTTP è un'attività fondamentale per comunicare tra client e server quando si sviluppano applicazioni web. Può essere utilizzato per richiedere dati, inviare informazioni o eseguire azioni su un server remoto.

## Come fare

Per inviare una richiesta HTTP in C, è necessario utilizzare la libreria standard ```<curl.h>```. Una volta inclusa la libreria nel codice, è possibile utilizzare le funzioni messe a disposizione per creare e inviare una richiesta HTTP. Di seguito un esempio di codice che invia una richiesta POST con dei parametri alla URL specificata:

```
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;
    
    // inizializza la libreria libcurl
    curl = curl_easy_init();
    
    if(curl) {
        // imposta la URL di destinazione
        curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
        
        // specifica che si desidera inviare una richiesta POST
        curl_easy_setopt(curl, CURLOPT_POST, 1L);

        // imposta i parametri da inviare
        // in questo caso, nome e cognome
        // utilizzati come esempio
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "name=John&surname=Smith");

        // invia la richiesta e salva il risultato nella variabile "res"
        res = curl_easy_perform(curl);

        // pulisci la struttura CURL
        curl_easy_cleanup(curl);
        
        // stampa il codice di ritorno della richiesta
        printf("Codice di ritorno: %d\n", res);
    }

    return 0;
}
```

L'esempio sopra invia una richiesta POST alla URL https://www.example.com inviando i parametri "name=John" e "surname=Smith". Il codice di ritorno della richiesta viene stampato a schermo.

## Approfondimento

Come detto in precedenza, per inviare una richiesta HTTP in C è necessario utilizzare la libreria ```<curl.h>```. All'interno di questa libreria ci sono numerose funzioni disponibili per creare, inviare e gestire le richieste HTTP. È possibile specificare vari parametri che controllano il comportamento della richiesta, come ad esempio il tipo (GET, POST, PUT, etc.), i parametri da inviare, gli header della richiesta e molto altro.

Un'altra caratteristica importante di libcurl è che supporta un'ampia gamma di protocolli, tra cui HTTP, HTTPS, FTP, SMTP, POP3 e molti altri.

## Vedi anche

- Documentazione ufficiale di libcurl: https://curl.haxx.se/libcurl/
- Altri esempi di codice per inviare richieste HTTP in C: https://www.programmingalgorithms.com/algorithm/http-client/c/