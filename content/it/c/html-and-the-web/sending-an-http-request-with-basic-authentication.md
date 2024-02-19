---
aliases:
- /it/c/sending-an-http-request-with-basic-authentication/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:05.821170-07:00
description: "Inviare una richiesta HTTP con autenticazione di base in C comporta\
  \ la creazione di una richiesta HTTP che include un'intestazione di Autorizzazione\
  \ con\u2026"
lastmod: 2024-02-18 23:08:56.338164
model: gpt-4-0125-preview
summary: "Inviare una richiesta HTTP con autenticazione di base in C comporta la creazione\
  \ di una richiesta HTTP che include un'intestazione di Autorizzazione con\u2026"
title: Inviare una richiesta HTTP con autenticazione di base
---

{{< edit_this_page >}}

## Cosa & Perché?
Inviare una richiesta HTTP con autenticazione di base in C comporta la creazione di una richiesta HTTP che include un'intestazione di Autorizzazione con le credenziali dell'utente codificate in Base64. Questo è un metodo comune per aggiungere un semplice strato di autenticazione alle richieste HTTP, consentendo di accedere programmaticamente a risorse riservate.

## Come fare:
Per inviare una richiesta HTTP con autenticazione di base in C, dobbiamo utilizzare la libreria libcurl, una libreria versatiele, popolare e facile da usare per il trasferimento di URL lato client. Gestisce vari protocolli, inclusi HTTP e HTTPS, semplificando il nostro compito. Assicurarsi che libcurl sia installato nel sistema prima di procedere. Ecco un esempio base che dimostra come inviare una richiesta GET con autenticazione di base:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        // L'URL a cui viene inviata la richiesta
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        // Abilitazione dell'uso dell'autenticazione di base
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        // Fornitura del nome utente e della password per l'autenticazione di base
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

        // Esecuzione della richiesta GET
        res = curl_easy_perform(curl);

        // Verifica degli errori
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));

        // Pulizia finale
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();

    return 0;
}
```
Nell'esempio sopra, sostituire `"http://example.com/resource"`, `"username"` e `"password"` con il proprio URL, nome utente e password effettivi.

Questo codice inizializza un oggetto `CURL`, imposta l'URL, abilita l'autenticazione HTTP di base e specifica le credenziali. Invia quindi la richiesta e si ripulisce dopo l'uso. Se ha successo, la risorsa richiesta viene recuperata; se c'è un errore, viene stampato in stderr.

L'output dell'esempio (assumendo un'autenticazione e l'accesso alla risorsa riusciti) potrebbe non essere mostrato direttamente dal programma, poiché l'esempio dimostra principalmente l'invio della richiesta. Per stampare la risposta, estenderesti il programma per gestire i dati della risposta HTTP.

## Approfondimento:
Inviare richieste HTTP con autenticazione di base in C, come mostrato, sfrutta la libreria libcurl per la sua robustezza e semplicità. Storicamente, creare richieste HTTP puramente in C senza tali librerie era oneroso e soggetto ad errori, coinvolgendo la programmazione a livello di socket e la costruzione manuale delle intestazioni HTTP.

L'autenticazione di base di per sé è un metodo dagli albori del web. Invia le credenziali in un formato facilmente decodificabile (Base64), il che è intrinsecamente insicuro su canali in chiaro. Le applicazioni moderne spesso preferiscono metodi di autenticazione più sicuri, come OAuth 2.0 o JWT (JSON Web Tokens), specialmente per dati sensibili.

Tuttavia, per sistemi interni, meno critici, o script veloci e sporchi dove la convenienza supera le preoccupazioni di sicurezza, l'auth di base rimane in uso. Inoltre, quando combinata con connessioni crittografate (HTTPS), la sua semplicità diventa un vantaggio per lo sviluppo rapido, i test o il lavoro di automazione dove meccanismi di sicurezza di livello superiore non sono così necessari.

Nei contesti in cui la sicurezza all'avanguardia è inderogabile, andrebbero privilegiate alternative come l'autenticazione basata su token. Comunque, capire come implementare l'auth di base in C tramite libcurl fornisce una competenza fondamentale che può essere adattata a vari metodi di autenticazione e protocolli, riflettendo i compromessi sfumati tra sicurezza, convenienza e requisiti applicativi nello sviluppo web.
