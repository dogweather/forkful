---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Inviare una richiesta HTTP è il mezzo fondamentale per lo scambio di dati e richieste tra client e server sul web. I programmatori lo fanno per richiedere dati da un server, inviare nuovi dati, aggiornare i dati esistenti o eliminarli.

## Come fare:

Un modo semplice per inviare una richiesta HTTP in C è l'uso della libreria `curl`. Installala prima utilizzando un gestore di pacchetti come `apt` per Ubuntu:
```C
sudo apt-get install curl
```
Poi includila nel tuo file `c`:
```C
#include <curl/curl.h>
```
Ecco un esempio che mostra come inviare una richiesta GET:
```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
    res = curl_easy_perform(curl);

    if(res != CURLE_OK) {
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
    }
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

## Approfondimento

Inviare richieste HTTP è una pratica comune fin dalla nascita del web. Prima dell'avvento delle librerie come `libcurl`, i programmatori di solito scrivevano la loro implementazione utilizzando i socket TCP, che è una procedura più laboriosa e complessa.

Sebbene `curl` sia tra le opzioni più popolari, esistono molte altre librerie che puoi utilizzare per inviare richieste HTTP in C, come `libwww`, `Soup` di GNOME e `C++ REST SDK` di Microsoft.

Se vai alle implementazioni, i dettagli specifici della richiesta HTTP saranno determinati dalle specifiche HTTP che stai seguendo (ad esempio, HTTP / 1.0, HTTP / 1.1 o HTTP / 2) e dal metodo utilizzato (ad esempio, GET, POST, PUT, DELETE). 

## Per ulteriori informazioni

1. [Documentazione ufficiale di libcurl](https://curl.se/libcurl/c/)
2. [Riferimenti HTTP da Mozilla](https://developer.mozilla.org/it/docs/Web/HTTP)
3. [Recap della documentazione HTTP](https://www.w3.org/Protocols/rfc2616/rfc2616.html)
4. [Overview delle librerie C HTTP](https://www.codediesel.com/c/5-http-library-for-c/)