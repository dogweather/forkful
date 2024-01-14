---
title:                "C: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

In questo articolo scopriremo come inviare richieste HTTP utilizzando il linguaggio di programmazione C. Le richieste HTTP sono una parte fondamentale delle applicazioni web moderne e capire come inviarle è essenziale per qualsiasi programmatore.

## Come fare

Per inviare una richiesta HTTP in C, è necessario utilizzare la libreria standard "curl". Prima di tutto, è necessario includere l'intestazione e impostare le opzioni di configurazione di base:

```C
#include <curl/curl.h>

// Opzioni di configurazione per la richiesta
CURL *curl;
CURLcode res;
curl = curl_easy_init();
```

Successivamente, specificare l'URL di destinazione e il metodo HTTP utilizzato nella richiesta. In questo esempio, utilizzeremo una richiesta GET semplice all'URL "https://www.example.com":

```C
curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
curl_easy_setopt(curl, CURLOPT_HTTPGET, 1L);
```

Infine, eseguire la richiesta e gestire la risposta ricevuta. Ad esempio, per stampare il codice di stato della risposta, è possibile utilizzare il seguente codice:

```C
// Eseguire la richiesta
res = curl_easy_perform(curl);

// Controllare il codice di stato della risposta
long http_code = 0;
curl_easy_getinfo (curl, CURLINFO_RESPONSE_CODE, &http_code);
printf("Codice di stato HTTP: %ld\n", http_code);
```

## Approfondimento

Oltre all'utilizzo delle funzioni descritte sopra, è anche possibile impostare un'opzione "write function" per gestire la risposta in modo più dettagliato:

```C
// Funzione di scrittura per la risposta
size_t write_function(void *buffer, size_t size, size_t nmemb, void *userp) {
  // Il buffer contiene i dati ottenuti dalla risposta
  // Size indica la lunghezza di ogni "chunk" di dati
  size_t byte_size = size * nmemb;

  // Fai qualcosa con i dati qui
  return byte_size;
}

// Imposta l'opzione per utilizzare la funzione di scrittura
curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_function);
curl_easy_setopt(curl, CURLOPT_WRITEDATA, your_data_ptr);
```

## Vedi anche
- [Documentazione CURL su invio di richieste](https://curl.se/libcurl/c/example.html)
- [Guida introduttiva alle richieste HTTP in C](https://www.codingame.com/playgrounds/14213/how-to-play-with-strings-in-c/richieste-http-con-curl)