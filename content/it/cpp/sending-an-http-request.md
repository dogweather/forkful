---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

In poche parole, inviare una richiesta HTTP significa comunicare con un server remoto per ottenere informazioni o eseguire un'azione. I programmatori spesso utilizzano le richieste HTTP per integrare i loro programmi con servizi esterni o per ottenere dati da fonti online.

## Come fare:

Di seguito sono presentati due esempi di codice che mostrano come inviare una richiesta HTTP utilizzando il linguaggio di programmazione C++. I codici sono scritti nel formato dei blocchi di codice ```C++ ... ``` per facilitare la lettura e la comprensione.

### Esempio 1: Richiesta GET

```C++
#include <iostream>
#include <curl/curl.h> //libreria per effettuare richieste HTTP

int main(){
  CURL *curl; //puntatore a oggetto curl
  CURLcode res; //variabile per gestire codici di stato

  //inizializzazione di curl
  curl = curl_easy_init();
  if(curl) {
    //impostazione dell'URL della richiesta
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/api/data");

    //esecuzione della richiesta GET
    res = curl_easy_perform(curl);

    //controllo del codice di stato
    if(res != CURLE_OK)
      std::cout << "Errore durante la richiesta HTTP: " << curl_easy_strerror(res) << std::endl;
    else
      std::cout << "Richiesta GET effettuata con successo!" << std::endl;
    
    //rilascio delle risorse
    curl_easy_cleanup(curl);
  }

  return 0;
}
```
Output:

```
Richiesta GET effettuata con successo!
```

### Esempio 2: Richiesta POST con dati

```C++
#include <iostream>
#include <curl/curl.h>

int main(){
  CURL *curl;
  CURLcode res;

  //impostazione dei dati da inviare
  std::string postFields = "user=test&password=1234";

  //inizializzazione di curl
  curl = curl_easy_init();
  if(curl) {
    //impostazione dell'URL della richiesta
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/api/login");

    //impostazione del metodo di richiesta
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "POST");

    //impostazione dei dati da inviare
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, postFields.c_str());

    //esecuzione della richiesta
    res = curl_easy_perform(curl);

    //controllo del codice di stato
    if(res != CURLE_OK)
      std::cout << "Errore durante la richiesta HTTP: " << curl_easy_strerror(res) << std::endl;
    else
      std::cout << "Richiesta POST effettuata con successo!" << std::endl;

    //rilascio delle risorse
    curl_easy_cleanup(curl);
  }

  return 0;
}
```
Output:

```
Richiesta POST effettuata con successo!
```

## Approfondimenti:

Le richieste HTTP sono state introdotte nel 1991 da Tim Berners-Lee, il fondatore del World Wide Web, per facilitare la comunicazione tra client e server. Esistono anche altri metodi di richiesta oltre a GET e POST, come ad esempio PUT e DELETE, che permettono di aggiornare o cancellare informazioni da un server. Inoltre, esistono anche diverse librerie e framework che facilitano l'invio di richieste HTTP, come ad esempio cURL, usato negli esempi sopra riportati.

## Vedi anche:

- [Documentazione di cURL](https://curl.haxx.se/docs/)
- [Tutorial su richieste HTTP in C++](https://isocpp.org/wiki/faq/ctors#static-init-order-on-first-use)
- [Esempi di richieste HTTP con Boost.Beast](https://www.boost.org/doc/libs/1_68_0/libs/beast/doc/html/beast/quick_start.html#beast.quick_start.request)