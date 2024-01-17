---
title:                "Scaricare una pagina web."
html_title:           "C: Scaricare una pagina web."
simple_title:         "Scaricare una pagina web."
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Scaricare una pagina web è il processo di acquisizione del contenuto di una pagina web dal server remoto e visualizzarlo sul tuo dispositivo locale. I programmatori spesso scaricano pagine web per analizzarne il contenuto o per utilizzarne le informazioni in altri progetti.

## Come:
Un modo semplice per scaricare una pagina web in C è utilizzare la libreria standard "curl". Di seguito è riportato un esempio di codice che utilizza curl per scaricare la pagina principale di Google:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  char *web_page = "https://www.google.com";
  curl = curl_easy_init();
  if(curl) {
    CURLcode res;
    curl_easy_setopt(curl, CURLOPT_URL, web_page);
    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

L'output di questo codice sarà il contenuto della pagina Google sulla console.

## Approfondimento:
Scaricare una pagina web è stato uno dei compiti più importanti degli inizi di Internet, quando era necessario un modo per condividere informazioni tra computer remoti. Oggi ci sono molti modi diversi per scaricare una pagina web, come utilizzare librerie di terze parti o creare il proprio parser HTML. Inoltre, è importante considerare la sicurezza durante il download di una pagina web, poiché potrebbero essere presenti codici dannosi che possono influenzare il tuo dispositivo.

## Vedi anche:
- Documentazione ufficiale di curl: <https://curl.haxx.se/libcurl/c>
- Tutorial su come scaricare una pagina web in C utilizzando libcurl: <https://www.digitalocean.com/community/tutorials/how-to-scrape-web-pages-with-c-and-libcurl>