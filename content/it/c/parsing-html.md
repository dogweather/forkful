---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:30:04.186097-07:00
simple_title:         "Analisi dell'HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/parsing-html.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Il parsing HTML significa analizzare il codice di una pagina web per estrarre dati specifici. I programmatori lo fanno per automatizzare l'interazione con siti web, sfruttare informazioni, o integrare contenuti web nelle loro applicazioni.

## Come Fare:
Utilizzeremo libcurl per il download dell'HTML e la libreria libxml2 per il parsing.

```C
#include <stdio.h>
#include <curl/curl.h>
#include <libxml/HTMLparser.h>

static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((char *)userp)[size * nmemb] = '\0'; // Assicurarsi di terminare la stringa
    return size * nmemb;
}

int main(void) {
    CURL *curl;
    CURLcode res;
    char buffer[100000]; // Un buffer abbastanza grande per contenere l'HTML

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, buffer);
        
        res = curl_easy_perform(curl);
        if(res == CURLE_OK) {
            htmlParserCtxtPtr ctxt = htmlNewParserCtxt();
            if(ctxt != NULL) {
                htmlDocPtr doc = htmlCtxtReadMemory(ctxt, buffer, strlen(buffer), NULL, NULL, HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);
                // Fai il parsing di doc qui ...
            }
        }
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
*Ricordati di linkare contro `libcurl` e `libxml2` quando compili.*

## Approfondimento:
Il parsing HTML richiede attenzione perché HTML nel mondo reale è spesso non ben formato. Le librerie come libxml2 sono robuste e affrontano queste imperfezioni. Nel tempo, oltre a metodi standard come DOM e SAX per il parsing, sono fiorite librerie come Beautiful Soup per Python o jsoup per Java. Per il C, usiamo spesso libxml2 perché è esaustiva e conforme agli standard.

Libcurl è praticamente uno standard de facto per il trasferimento dati via URL, mentre libxml2 domina nel parsing di XML e HTML. Le alternative implicano spesso scrivere codice da zero per gestire casi limite, il che sarebbe oneroso e inaffidabile.

## Vedi Anche:
- Documentazione libxml2: http://xmlsoft.org/
- Tutorial libcurl: https://curl.haxx.se/libcurl/c/
- W3 HTML Parsing Rules: https://www.w3.org/TR/html5/syntax.html#parsing
- Overflow di stack su argomenti di parsing HTML in C: https://stackoverflow.com/questions/tagged/html-parsing+c
