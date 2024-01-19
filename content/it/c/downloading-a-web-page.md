---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Scaricare una Pagina Web in C: una Guida Pratica

## Cos'è & Perché?
Scaricare una pagina web significa recuperare il suo codice HTML attraverso la rete. I programmatori fanno ciò per analizzare i dati, automatizzare le attività o controllare le modifiche.

## Come fare:
Per scaricare una pagina web in C, puoi usare la libreria cURL. Ecco il codice di esempio:
```C
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *buffer, size_t size, size_t nmemb, void *userp)
{
   return fwrite(buffer, size, nmemb, (FILE *)userp);
}

int main(void)
{
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://www.example.com";
    char outfilename[FILENAME_MAX] = "baixado.html";
    curl = curl_easy_init();
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
        fclose(fp);
    }
    return 0;
}
```
Questo programma scarica il contenuto HTML di www.example.com in un file chiamato "baixado.html"

## Scavo Profondo
La libreria cURL, introdotta nel 1997, è diventata uno standard de facto per il download di pagine web in C. Remota HTTP, HTTPS, FTP, tra gli altri. 

Nonostante cURL sia molto popolare, esistono altre librerie come libwww o libsoup che offrono funzionalità simili. Ad esempio, libwww supporta le operazioni di rete asincrone, cosa che cURL no.

In termini di implementazione, cURL apre una connessione TCP al server web e invia una richiesta HTTP GET. Il server risponde con i dati della pagina web, che vengono quindi scritti nel file di output.

## Vedi Anche
1. Documentazione di cURL: https://curl.se/libcurl/c/
2. Introduzione alle richieste di rete in C: https://beej.us/guide/bgnet/
3. Libwww: http://www.w3.org/Library/
4. Libsoup: https://developer.gnome.org/libsoup/stable/