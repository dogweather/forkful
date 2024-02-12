---
title:                "Scaricare una pagina web"
date:                  2024-02-03T17:55:46.747417-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scaricare una pagina web"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Scaricare una pagina web in C comporta l'accesso programmato al contenuto di una pagina web tramite internet e il suo salvataggio localmente per l'elaborazione o l'uso offline. I programmatori spesso si impegnano in questo per consumare servizi web, raschiare contenuti web o interagire direttamente con risorse online dalle loro applicazioni.

## Come fare:

Per scaricare una pagina web in C, un approccio popolare è l'utilizzo della libreria libcurl, una libreria di trasferimento URL lato client efficiente e portabile. Assicurati di avere libcurl installato e collegato nel tuo progetto. Ecco un esempio che dimostra come utilizzare libcurl per scaricare il contenuto di una pagina web:

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "./downloaded_page.html";

    curl = curl_easy_init(); // Inizializza una sessione facile di libcurl
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // Callback per scrivere i dati ricevuti
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // Imposta il puntatore al file per scrivere i dati

        res = curl_easy_perform(curl); // Esegue il download del file
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() non riuscito: %s\n",
                    curl_easy_strerror(res));
        }

        /* pulizia sempre */
        curl_easy_cleanup(curl); // Pulisce la sessione facile
        fclose(fp); // Chiude il flusso del file
    }
    return 0;
}
```
Risultato dell'esempio (nessun output visibile nella console): Questo codice scarica il contenuto all'URL specificato e lo salva in un file denominato `downloaded_page.html`. Verifica la directory del tuo programma per questo file per vedere il contenuto scaricato.

## Approfondimento:

Storicamente, scaricare contenuti web in C era più laborioso, richiedendo la programmazione manuale dei socket e la gestione del protocollo HTTP. Libcurl astrae queste complessità, offrendo una API robusta e di alto livello per il trasferimento di dati tramite il web.

Sebbene libcurl semplifichi le richieste HTTP in C, linguaggi di programmazione moderni come Python con la loro libreria `requests` o JavaScript (Node.js) con varie librerie client HTTP possono offrire una sintassi più intuitiva e supporto incorporato per JSON e altri formati di dati comunemente utilizzati nella comunicazione web. Tuttavia, C e libcurl forniscono una soluzione ad alte prestazioni e stabile per sistemi dove l'efficienza, il controllo dettagliato o l'integrazione in codebase C esistenti sono critici. Vale anche la pena notare che C, combinato con libcurl, può essere utilizzato per molto più che semplicemente scaricare pagine web: è capace di gestire FTP, SMTP e molto altro, rendendolo uno strumento versatile nel kit di un programmatore.
