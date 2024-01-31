---
title:                "Inviare una richiesta http con autenticazione di base"
date:                  2024-01-20T18:01:00.062613-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http con autenticazione di base"

category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa è e Perché?)
Mandare una richiesta HTTP con autenticazione base significa inserire username e password per accedere a risorse protette. I programmatori lo fanno per interagire con web services che richiedono identificazione.

## How to: (Come fare)
Utilizziamo `libcurl` per l'esempio, una libreria diffusa per le richieste HTTP in C. Prima, installa `libcurl` se non l'hai già.

Ecco un esempio minimale:

```C
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;
    
    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://tuo.server/risorsa");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "tuo_username");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "tua_password");
        
        // Perform the request, res will get the return code
        res = curl_easy_perform(curl);
        // Check for errors
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        
        // Cleanup
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();
    return 0;
}
```

Eseguilo e dovresti ottenere l'output della risorsa protetta.

## Deep Dive (Approfondimento)
L'autenticazione HTTP basic è un sistema di sicurezza semplice ma non molto sicuro; la username e la password sono codificate in Base64 senza crittografia, quindi usa HTTPS per proteggere le credenziali.

Nato nei primi giorni del web, il meccanismo di base è stato poi superato da sistemi più robusti come OAuth. Tuttavia, per semplici API o ambienti controllati, l'autenticazione base è ancora in uso.

`libcurl` offre funzionalità oltre l'autenticazione base come SSL/TLS, cookie, e molto altro.

## See Also (Vedi anche)
- Documentazione di libcurl: https://curl.se/libcurl/
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- Tutorial HTTPS con libcurl: https://curl.se/docs/sslcerts.html
- OAuth: https://oauth.net/
