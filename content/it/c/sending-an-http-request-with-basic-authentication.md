---
title:                "C: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché inviare una richiesta HTTP con autenticazione di base?

L'invio di una richiesta HTTP con autenticazione di base è una pratica comune nei linguaggi di programmazione come C. Questo metodo di autenticazione è utilizzato per proteggere le informazioni sensibili durante la comunicazione tra un client e un server. Se stai lavorando su un progetto che prevede l'uso di un servizio web o di una API che richiedono autenticazione, dovrai conoscere come inviare una richiesta con autenticazione di base.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base in C, è necessario seguire alcuni semplici passaggi:

1. Includi la libreria `curl` nel tuo codice. `#include <curl/curl.h>`
2. Definisci un nome utente e una password per l'autenticazione. `char *username = "nome_utente";` e `char *password = "password";`
3. Costruisci la stringa di autenticazione base64. `char auth[50]; sprintf(auth, "%s:%s", username, password);`
4. Codifica la stringa di autenticazione in base64. Puoi utilizzare la funzione `curl_base64_encode()` della libreria `curl`. `char *encoded_auth = curl_base64_encode(auth, strlen(auth));`
5. Costruisci la richiesta HTTP con l'intestazione `Authorization` contenente la stringa di autenticazione codificata. `CURL *curl = curl_easy_init(); struct curl_slist *headerlist = NULL; headerlist = curl_slist_append(headerlist, "Authorization: Basic " + encoded_auth);`
6. Esegui la richiesta con `curl_easy_perform()`. `curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headerlist); curl_easy_setopt(curl, CURLOPT_URL, "url_del_server"); res = curl_easy_perform(curl);`

Ecco un esempio completo di codice che invia una richiesta GET con autenticazione di base:

```
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  char *username = "nome_utente";
  char *password = "password";

  char auth[50];
  sprintf(auth, "%s:%s", username, password);

  char *encoded_auth = curl_base64_encode(auth, strlen(auth));

  curl = curl_easy_init();
  if(curl) {
    struct curl_slist *headerlist = NULL;
    headerlist = curl_slist_append(headerlist, "Authorization: Basic " + encoded_auth);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headerlist);
    curl_easy_setopt(curl, CURLOPT_URL, "url_del_server");
    res = curl_easy_perform(curl);
    curl_easy_cleanup(curl);
  }

  return 0;
}
```

Ecco un esempio di output che puoi ottenere inviando una richiesta HTTP con autenticazione di base:

```
Following this link will give the following result:

Authentication: Basic YXdlc29tZV91c2VybmFtZTpwYXNzd29yZA==

status: 200 OK
connection: close
```

## Approfondimenti

Oltre all'autenticazione di base, ci sono anche altri metodi di autenticazione che possono essere utilizzati nelle richieste HTTP. Alcuni esempi includono l'autenticazione con chiavi API e l'autenticazione OAuth. Conoscere le diverse opzioni di autenticazione e quando utilizzarle è importante per la sicurezza delle tue applicazioni e dei tuoi servizi web.

Se vuoi saperne di più su come lavorare con richieste HTTP in C, puoi consultare questi link:

- [Documentazione ufficiale di Curl](https://curl.se/libcurl/c/http-authentication.html)
- [Tutorial su come inviare richieste HTTP in C con la libreria libcurl](https://dzone.com/articles/libcurl-tutorial-with-code-examples)
- [Tutorial su come utilizzare vari tipi di autenticazione in Curl](https://www.codeproject.com/Articles/546398/Http-Authentication-with-Cplusplus-and-Curl-librar)

## Vedi anche

- [Come inviare una richiesta HTTP in Python con autenticazione di base (articolo in italiano)](https://www.apprendipython.com/come-inviare-una-richiesta-http-in-python/)