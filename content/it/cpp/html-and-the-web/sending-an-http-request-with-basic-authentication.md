---
title:                "Inviare una richiesta http con autenticazione di base"
aliases: - /it/cpp/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:27.725201-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http con autenticazione di base"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Inviare una richiesta HTTP con autenticazione di base significa trasmettere username e password in formato codificato per accedere a risorse protette. I programmatori usano questo metodo per interagire con API che richiedono autenticazione per ottenere dati o eseguire operazioni.

## How to:
Per inviare una richiesta HTTP con autenticazione di base in C++, possiamo usare la libreria `cURL`. Ecco un esempio di codice:

```C++
#include <iostream>
#include <curl/curl.h>
#include <string>

static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        std::string userPwd = "username:password"; // Sostituisci con le tue credenziali
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, userPwd.c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        
        res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            std::cerr << "curl_easy_perform() failed: " << curl_easy_strerror(res) << std::endl;
        } else {
            std::cout << "Output: " << readBuffer << std::endl;
        }
        
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Output esemplificativo:
```
Output: { "message": "Benvenuto all'API! Le tue credenziali sono valide." }
```

## Deep Dive
L'uso di autenticazione di base risale agli albori del web. Mentre è semplice da implementare, oggi non è considerato molto sicuro, poiché le credenziali codificate in base64 possono essere facilmente decodificate. Dove è richiesta maggiore sicurezza, si preferisce l'utilizzo di token di autenticazione, OAuth, o altri meccanismi più robusti.

Nell'esempio fornito, `libcurl` si occupa della complessità della comunicazione HTTP. Fornisce un'interfaccia per inviare richieste con vari metodi di autenticazione, ma occorre dedicare attenzione alla gestione della memoria e alla pulizia delle risorse (`curl_easy_cleanup`).

L'uso di callback, come `WriteCallback`, serve per gestire i dati ricevuti in risposta. È possibile personalizzare questi handler per adattarli alle specifiche esigenze dell'applicativo.

## See Also
- Documentazione ufficiale di libcurl: https://curl.se/libcurl/c/
- Wikipedia HTTP Basic Authentication: https://it.wikipedia.org/wiki/Basic_access_authentication
- RFC 7617 (The 'Basic' HTTP Authentication Scheme): https://tools.ietf.org/html/rfc7617
