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

Invio di una richiesta HTTP significa comunicare con un server per richiedere o ricevere informazioni. I programmatori lo fanno per interagire con API web, scaricare file, e eseguire altre operazioni di rete.

## Come si fa:

C++ moderno non ha una libreria standard HTTP, quindi useremo la libreria cURL. Prima, installa cURL (disponibile [qui](https://curl.se/download.html)).

```C++
#include <curl/curl.h>

int main() 
{
    CURL *curl = curl_easy_init();
    if(curl) {
        CURLcode res;
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
La codice sopra invia una richiesta GET al sito "http://example.com". 

## Approfondimento

La funzionalità HTTP non è sempre stata parte di C++. Nel passato, i programmatori utilizzavano libreria come `libcurl` o `Boost.Asio` per inviare richieste HTTP.

Alcune alternative moderne includono le librerie `cpp-httplib` e `Boost.Beast`. `cpp-httplib` offre un'interfaccia semplice ed intuitiva, mentre `Boost.Beast` è noto per la sua potenza e flessibilità.

Il codice che abbiamo visto sfrutta le funzioni libcurl. `curl_easy_init` inizializza una sessione cURL; `curl_easy_setopt` imposta le opzioni per la sessione (n.r. l'URL per la richiesta); `curl_easy_perform` esegue la richiesta; infine, `curl_easy_cleanup` chiude la sessione.

## Approfondisci

Per saperne di più su c++ networking e invio di richieste HTTP, consulta queste risorse:

- Documentazione libcurl: [qui](https://curl.se/libcurl/c/)
- C++ httplib - [github](https://github.com/yhirose/cpp-httplib)
- Boost.Beast - [boost](https://www.boost.org/doc/libs/1_70_0/libs/beast/doc/html/index.html)