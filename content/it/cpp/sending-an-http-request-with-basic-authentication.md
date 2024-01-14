---
title:                "C++: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché
In questo articolo scoprirai perché inviare una richiesta HTTP con un'autenticazione di base può essere utile per accedere in modo sicuro a risorse online.

## Come Fare
Per inviare una richiesta HTTP con autenticazione di base in C++, è necessario seguire questi passaggi:

1. Includi la libreria `curl` nel tuo progetto C++.
2. Imposta l'URL della risorsa a cui desideri accedere.
3. Imposta il tipo di richiesta HTTP (GET, POST, PUT, etc.).
4. Aggiungi le credenziali dell'utente per l'autenticazione di base.
5. Esegui la richiesta utilizzando la funzione `curl_easy_perform()`.

Di seguito troverai un esempio di codice che invia una richiesta HTTP GET con autenticazione di base e stampa il risultato nel terminale:

```C++
#include <curl/curl.h>

int main()
{
    // Set URL
    const char* url = "https://example.com/resource";

    // Create CURL easy handle
    CURL* curl = curl_easy_init();

    if(curl)
    {
        // Set URL and HTTP request type
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "GET");

        // Set basic authentication credentials
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

        // Perform HTTP request
        CURLcode res = curl_easy_perform(curl);

        // Check for errors
        if(res != CURLE_OK)
        {
            std::cout << "Error: " << curl_easy_strerror(res) << std::endl;
        }
        else
        {
            // Print result
            std::cout << curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE) << std::endl;
        }

        // Cleanup
        curl_easy_cleanup(curl);
    }

    return 0;
}
```

Output:
```
200 OK
```

## Approfondimento
L'autenticazione di base è uno dei metodi più semplici per proteggere le risorse online tramite autenticazione. Quando viene inviata una richiesta HTTP con autenticazione di base, il server richiederà le credenziali dell'utente per permettere l'accesso alla risorsa. Le credenziali vengono poi codificate in Base64 e inviate insieme alla richiesta. Questo metodo di autenticazione può essere utilizzato per accedere a risorse come API, server FTP e pagine web protette.

## Vedi Anche
- [Documentazione CURL](https://curl.haxx.se/libcurl/)
- [RFC 2617 - HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
- [Base64 encoding](https://en.wikipedia.org/wiki/Base64)