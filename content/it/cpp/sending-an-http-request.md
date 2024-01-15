---
title:                "Inviare una richiesta http."
html_title:           "C++: Inviare una richiesta http."
simple_title:         "Inviare una richiesta http."
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

Molti moderni linguaggi di programmazione, tra cui C++, offrono un modo semplice per inviare una richiesta HTTP: questo consente agli sviluppatori di interagire con server remoti e accedere a dati e servizi esterni.

## Come Fare

Per inviare una richiesta HTTP in C++, il primo passo è includere la libreria "iostream". Successivamente, si crea un oggetto "string" contenente l'URL del server a cui si vuole inviare la richiesta.

```
#include <iostream>
#include <string>

int main()
{
  // Creazione dell'oggetto string con l'URL
  std::string url = "https://www.example.com";

  // Codice per inviare la richiesta HTTP
}
```

Dopo aver creato l'oggetto "string", è necessario utilizzare la libreria "curl" per configurare e inviare la richiesta. Ciò può essere fatto tramite le funzioni "curl_easy_init()" e "curl_easy_perform()", passando l'URL come parametro.

```
#include <iostream>
#include <string>
#include <curl/curl.h>

int main()
{
  // Creazione dell'oggetto string con l'URL
  std::string url = "https://www.example.com";

  // Inizializzazione di curl
  CURL *curl;
  curl = curl_easy_init();

  // Invio della richiesta HTTP all'URL specificato
  curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
  curl_easy_perform(curl);
  curl_easy_cleanup(curl);
}
```

Una volta che la richiesta è stata inviata, è possibile visualizzare la risposta ottenuta dal server. Per fare ciò, è necessario utilizzare la funzione "curl_easy_getinfo()" e passare il parametro "CURLINFO_RESPONSE_CODE".

```
#include <iostream>
#include <string>
#include <curl/curl.h>

int main()
{
  // Creazione dell'oggetto string con l'URL
  std::string url = "https://www.example.com";

  // Inizializzazione di curl
  CURL *curl;
  curl = curl_easy_init();

  // Invio della richiesta HTTP all'URL specificato
  curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
  curl_easy_perform(curl);
  
  // Ottenimento del codice di risposta dal server
  long response_code;
  curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
  std::cout << "Codice di risposta del server: " << response_code << std::endl;
  
  curl_easy_cleanup(curl);
}
```

## Deep Dive

Per inviare una richiesta HTTP più complessa, è possibile utilizzare la funzione "curl_easy_setopt()" per impostare diversi parametri, come il tipo di richiesta (GET, POST, PUT, etc.), il corpo della richiesta e gli header.

Inoltre, la libreria "curl" offre anche una vasta gamma di opzioni avanzate per gestire la comunicazione con il server, come la gestione dei certificati SSL e dei cookie.

## Vedi Anche

- [Documentazione di cURL](https://curl.se/libcurl/c/)
- [Tutorial di programmazione in C++](https://www.tutorialspoint.com/cplusplus/index.htm)
- [Guida a cURL in C++](https://curl.haxx.se/libcurl/c/)