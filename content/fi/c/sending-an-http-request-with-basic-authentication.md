---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# HTTP-pyynnön lähettäminen perusautentikoinnilla C-kielellä  

## Mitä & Miksi?

HTTP-pyynnön lähettäminen perusautentikoinnilla on prosessi, jossa lähetetään pyyntö web-palvelimelle suojausvaltuutuksen kanssa. Sitä tarvitaan, jotta voitaisiin suorittaa turvallisia toimintoja, kuten tiedon haku tai päivitys web-palvelimelta.

## Miten:

Toteutus vaatii `libcurl`-kirjaston. Asenna se ensin. Otetaan esimerkkipainopisteenä tiedon haku palvelimelta:

```C
#include <curl/curl.h>
#include <stdlib.h>
#include <string.h>

#define URL "http://example.com"

int main(void) {
    CURL *curl;
    CURLcode res;
    
    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, URL);
        
        struct curl_slist *headers = NULL;
        headers = curl_slist_append(headers, "Authorization: Basic dXNlcjpwYXNzd29yZA==");
        
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
        
        res = curl_easy_perform(curl);
        
        if(res != CURLE_OK)
          fprintf(stderr, "curl_easy_perform() failed: %s\n",
                  curl_easy_strerror(res));
        
        curl_easy_cleanup(curl);
        curl_slist_free_all(headers);
    }
    
    curl_global_cleanup();
    
    return 0;
}
```

## Syvä sukellus

HTTP-pyynnön perusautentikointi ei ole uusi keksintö. Se on osa alkuperäistä HTTP/1.0 standardia, joka julkaistiin vuonna 1996. Vaikka se ei olekaan vahvin saatavilla oleva autentikointimenetelmä, sitä käytetään edelleen sen yksinkertaisuuden vuoksi. 

Vaihtoehtoja on monia, kuten OAuth ja JWT, mutta ne voivat vaatia enemmän integraatiota. Perusautentikoinnissa käytämme yksinkertaisesti Base64-koodattua `käyttäjänimi:salasana` -merkkijonoa, jonka lisäämme HTTP-pyynnön otsikkoon.

## Katso myös:

- [libcurl kirjasto](https://curl.haxx.se/libcurl/c/)
- [HTTP autentikointi](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Vaihtoehtoiset autentikointimenetelmät](https://auth0.com/learn/token-based-authentication-made-easy/)