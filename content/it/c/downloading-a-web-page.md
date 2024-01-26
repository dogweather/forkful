---
title:                "Scaricare una pagina web"
date:                  2024-01-20T17:43:29.330231-07:00
model:                 gpt-4-1106-preview
simple_title:         "Scaricare una pagina web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Scaricare una pagina web significa prelevare i dati da remoto per utilizzarli localmente. I programmatori fanno ciò per analizzare il contenuto, monitorare cambiamenti o integrare funzionalità web nelle loro applicazioni.

## How to:
Per scaricare una pagina web in C, usiamo libcurl, una libreria affidabile per le operazioni in rete.

```C
#include <stdio.h>
#include <curl/curl.h>

static size_t write_data(void *buffer, size_t size, size_t nmemb, void *userp) {
    fwrite(buffer, size, nmemb, (FILE *)userp);
    return size * nmemb;
}

int main(void) {
    CURL *curl = curl_easy_init();
    if(curl) {
        FILE *fp = fopen("output.html", "wb");
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        curl_easy_perform(curl);
        curl_easy_cleanup(curl);
        fclose(fp);
    }
    return 0;
}
```

Questo codice scarica il contenuto di http://example.com e lo salva in "output.html".

## Deep Dive:
Usare libcurl risale agli anni '90. Prima esistevano tecniche più primitive come l'apertura di socket TCP manuali e l'invio di richieste HTTP.

Le alternative includono:
- `sockets` API di basso livello se vuoi controllo totale.
- `wget` e `libwget`, comandi/utilità simili.

Dettagli implementativi:
- Gestisci gli errori! Il codice sopra è semplificato.
- `curl_easy_setopt` configura le opzioni di trasferimento.
- `write_data` è un callback per scrivere i dati ricevuti.
- È fondamentale liberare le risorse: `curl_easy_cleanup` e `fclose`.

## See Also:
- Documentazione di libcurl: https://curl.haxx.se/libcurl/
- Tutorial di C su sockets: https://beej.us/guide/bgnet/
- Lista di librerie HTTP per C: https://en.wikipedia.org/wiki/Comparison_of_HTTP_library_(programming)
