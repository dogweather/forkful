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

## Che cos'è e perché lo si fa? 
L'invio di una richiesta HTTP con autenticazione di base è un modo per accedere a risorse protette tramite un meccanismo di autenticazione semplice. I programmatori spesso lo fanno per verificare l'identità di un utente e garantire che solo chi è autorizzato possa accedere ai dati o alle funzionalità.

## Come fare: 
Ecco un esempio di codice in C++ che mostra come inviare una richiesta HTTP con autenticazione di base:

```C++
#include <iostream>
#include <curl/curl.h>
using namespace std;

int main() {
  // Inizializza il gestore di richieste CURL
  CURL *curl;
  curl = curl_easy_init();

  // Imposta l'autenticazione di base con nome utente e password
  curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
  curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
  curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");

  // Esegui la richiesta HTTP a un'URL di esempio
  curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com");
  CURLcode res = curl_easy_perform(curl);

  // Se la richiesta ha successo, stampa il risultato
  if (res == CURLE_OK) {
    cout << "Richiesta inviata con successo!";
  }

  // Dealloca la memoria e chiude il gestore di richieste CURL
  curl_easy_cleanup(curl);

  return 0;
}
```

Ecco un esempio di output:

```
Richiesta inviata con successo!
```

## Approfondimento:
Questa pratica è stata introdotta nei primi anni del World Wide Web come uno dei primi metodi per proteggere l'accesso alle risorse online. Oggi, ci sono anche altre forme di autenticazione più sicure, come OAuth, ma l'autenticazione di base continua ad essere utilizzata in situazioni in cui la sicurezza non è una preoccupazione primaria.

Per inviare una richiesta HTTP con autenticazione di base, è necessario specificare il metodo di autenticazione nel codice e fornire le credenziali dell'utente autorizzato. È importante anche indicare l'URL a cui si desidera accedere e gestire eventuali errori che potrebbero verificarsi durante l'invio della richiesta.

## Vedi anche:
- [C++ CURL: l'esempio più semplice per fare richieste HTTP in C++](https://articles.cafe/cpp-curl/)
- [C++ Tutorial su autenticazione HTTP con libcurl](https://curl.haxx.se/libcurl/c/example.html)