---
title:                "Scaricando una pagina web"
html_title:           "C++: Scaricando una pagina web"
simple_title:         "Scaricando una pagina web"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Scaricare una pagina web significa ottenere il codice sorgente di una pagina web da internet e salvarlo sul nostro computer. I programmatori spesso fanno ciò per analizzare o manipolare il codice per creare applicazioni o per risolvere problemi tecnici.

## Come fare:
```C++
#include <iostream>
#include <curl/curl.h>

int main() {
  curl_global_init(CURL_GLOBAL_ALL);
  // inizializza la libreria libcurl
  
  CURL *curl = curl_easy_init();
  // creare la struct CURL 
  
  curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/");
  // impostare l'URL da cui scaricare il codice sorgente
  
  curl_easy_perform(curl);
  // eseguire la richiesta
  
  curl_easy_cleanup(curl);
  // pulire la struct CURL
  
  curl_global_cleanup();
  // pulire la libreria libcurl
  
  return 0;
}
```

Sample output:

```
<!doctype html>
<html>
<head>
  <title>Example Domain</title>
  <meta charset="utf-8" />
...
```

## Approfondimento:
Scaricare una pagina web può essere utile per recuperare informazioni specifiche o per creare applicazioni che interagiscono con il web. Prima dell'uso di librerie come libcurl, i programmatori dovevano scrivere il loro codice per aprire una connessione con un server web, inviare manualmente una richiesta HTTP e analizzare la risposta. Esistono anche alternative per scaricare pagine web, come l'utilizzo di browser automatizzati o web scraping.

## Vedi anche:
- [libcurl documentation](https://curl.se/libcurl/)
- [Web scraping with Python](https://realpython.com/beautiful-soup-web-scraper-python/)