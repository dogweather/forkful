---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Arduino: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Inviare una richiesta HTTP con autenticazione di base significa semplicemente creare una richiesta HTTP che include le credenziali (nome utente e password) in formato codificato. I programmatori fanno questo per accedere a determinate risorse protette su un server web.

## Come fare:
Ecco un esempio breve ma completo. Questo codice invia una richiesta GET ad un URL protetto con autenticazione di base.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *ch;
    CURLcode res;

    ch = curl_easy_init();

    if(ch) {
        struct curl_slist *headers=NULL;

        curl_easy_setopt(ch, CURLOPT_URL, "http://localhost:8080");
        curl_easy_setopt(ch, CURLOPT_USERNAME, "username");
        curl_easy_setopt(ch, CURLOPT_PASSWORD, "password");
        curl_easy_setopt(ch, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(ch, CURLOPT_HTTPHEADER, headers);

        res = curl_easy_perform(ch);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

        curl_easy_cleanup(ch);
    }

    return 0;
}
```

Se tutto va come previsto, non vedrai alcun output. In caso contrario, vedrai un messaggio di errore.

## Approfondimento
L’autenticazione di base HTTP è uno degli schemi di autenticazione più antichi. Venne introdotto con la specifica HTTP 1.0 nel 1996. Sebbene sia semplice da implementare, non è il più sicuro poiché invia le credenziali in chiaro (base64 encoded - facilmente decodificabile). Alternative includono OAuth2, Digest Authentication e l'uso di token JWT.

L'implementazione dell'invio di una richiesta HTTP con autenticazione di base in C avviene principalmente tramite la libreria cURL. È abbastanza potente e versatile, consentendo di inviare richieste GET, POST e altro ancora.

## Ancora Di Più
Se vuoi leggere di più sull'argomento, ecco alcuni link utili:

1. [curl - Tutorial punto per punto](https://curl.haxx.se/docs/httpscripting.html)
2. [GitHub - Documentazione API, Autenticazione](https://docs.github.com/en/rest/guides/getting-started-with-the-rest-api)
3. [RFC 2617 - Autenticazione HTTP](https://tools.ietf.org/html/rfc2617)
4. [HTTP: The Protocol Every Web Developer Must Know](https://code.tutsplus.com/tutorials/http-the-protocol-every-web-developer-must-know-part-1--net-31177)