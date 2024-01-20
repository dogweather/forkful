---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Artikkeli: HTTP-pyynnön lähettäminen C-ohjelmoinnissa

## Mikä & Miksi?
HTTP-pyyntö on tapa hakea tai lähettää tietoa verkkopalvelimelta tai siihen. Ohjelmoijat lähettävät näitä pyyntöjä, jotta he voivat vuorovaikuttaa verkkopalvelimien kanssa - esimerkiksi hakea verkkosivua tai lähettää tietoja.

## Miten tehdään:
Käytetään "libcurl" kirjastoa HTTP -pyynnön lähettämiseksi. Asennetaan se ensin.

```C
sudo apt-get install libcurl4-openssl-dev
```

Tässä on yksinkertainen koodiesimerkki.

```C
#include <curl/curl.h>

int main(void)
{
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                curl_easy_strerror(res));
        curl_easy_cleanup(curl);
    }
    curl_global_cleanup();
    return 0;
}
```
Tämä koodi lataa etusivun osoitteesta http://example.com.

## Syvemmälle
HTTP-pyyntö muodostettiin alun perin osana HTTP-protokollaa vuonna 1991. Vaihtoehtoisia tapoja HTTP-pyyntöjen tekemiseen ovat esimerkiksi Pythonin `requests` -kirjasto tai JavaScriptin Fetch API. Kuitenkin C-ohjelmointikielessä libcurl on yleisin valinta. Se on nopea, tehokas ja tukee monia protokollia.

Libcurlin yksityiskohdat: se käyttää 'CURL_GLOBAL_DEFAULT'-optiota alustaakseen globaalin ympäristön ja CURL-hallinnan. 'CURLOPT_URL' -asetus määrittää URL-osoitteen, johon pyyntö lähetetään. Käytetään 'curl_easy_perform' -funktiota suorittamaan varsinainen HTTP-pyyntö. Jos pyyntö epäonnistuu, tulostaa 'curl_easy_strerror' virheilmoituksen.

## Katso myös
Lisätietoa löydät seuraavilta sivustoilta:

1. [HTTP Protocol:](https://developer.mozilla.org/fi/docs/Web/HTTP)
2. [C-libcurl:](https://curl.haxx.se/libcurl/c/)
3. [Python requests:](https://docs.python-requests.org/en/latest/)
4. [Fetch API:](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)