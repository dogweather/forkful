---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "C: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Di cosa si tratta e perché?
Il comando per inviare una richiesta HTTP con autenticazione di base è un modo per fornire un livello di sicurezza aggiuntivo quando si interagisce con un server web. Questo significa che i programmatori possono proteggere le informazioni sensibili che vengono scambiate tra il client e il server. 

## Come fare:
Per inviare una richiesta HTTP con autenticazione di base in C, è necessario utilizzare la funzione `curl_easy_setopt()` e passare i parametri `CURLOPT_URL` e `CURLOPT_USERPWD` come nell'esempio seguente:

```
CURL *curl = curl_easy_init();
if(curl) {
  curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
  curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");
  CURLcode res = curl_easy_perform(curl);
  if(res != CURLE_OK)
    fprintf(stderr, "curl_easy_perform() failed: %s\n",
            curl_easy_strerror(res));
  curl_easy_cleanup(curl);
}
```

L'output di questa richiesta sarà un oggetto `CURLcode` contenente informazioni sull'esito della richiesta.

## Approfondimento:
L'autenticazione di base è uno dei primi metodi di autenticazione utilizzati su internet. Consiste nell'inviare le credenziali (nome utente e password) in chiaro, quindi non è sicuro come altri metodi di autenticazione come OAuth o OpenID Connect. Tuttavia, può essere utile per applicazioni interne o in situazioni in cui la sicurezza non è una priorità assoluta.

Come alternativa all'utilizzo di `curl_easy_setopt()`, è possibile utilizzare una libreria di terze parti o una funzione personalizzata per gestire l'autenticazione di base.

Per implementare l'autenticazione di base, il server web deve avere un meccanismo per verificare le credenziali fornite dal client. Ciò può essere fatto tramite una lista di utenti e password memorizzati nel server o attraverso l'utilizzo di un servizio di autenticazione centralizzato.

## Vedi anche:
- [Documentazione ufficiale di CURL](https://curl.haxx.se/libcurl/c/Using-cURL-in-C-code.html)
- [Tutorial su autenticazione di base con CURL in C](https://gist.github.com/subfuzion/08c5d85437d5d4f00e58)
- [Libreria CURL wrapper per C++](https://github.com/jpbarrette/curlpp)