---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ladda ner en webbsida innebär att hämta all data (HTML, CSS, JS, bilder) från en server till din dator. Programmerare gör detta för att analysera webbsidans data, testa deras program eller göra offline-versioner av sidor. 

## Så här gör du:

Använda biblioteket libcurl för att ladda ner en webbsida. Installera det först med `sudo apt install libcurl4-gnutls-dev`. 

```C
#include <stdio.h>
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

    /* always cleanup */ 
    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();

  return 0;
}
```

Kör programmet. Om det lyckas, visar den hela HTML-innehållet i http://example.com.

## Fördjupning

Hämta webbsidor blev populärt med början av internetåldern. Alternativ till libcurl inkluderar `wget` eller `HTTrack`, men de är inte inbyggda i C.

När det gäller implementeringsdetaljer, `libcurl` stöder HTTP/1.x, HTTP/2, HTTP/3 med QUIC, Cookies, User+Password authentication (Basic, Plain, Digest, CRAM-MD5, NTLM, Negotiate and Kerberos), proxies, DNS name resolving, and more. Detta gör det mycket kraftfullt men också komplicerat att använda fullt ut.

## Se också

Besök [`libcurl`](https://curl.haxx.se/libcurl/c/) officiella dokumentation för mer information och kodexempel. 

Studera mer om webb skrapning med [`Wget`](https://www.gnu.org/software/wget/) och [`HTTrack`](https://www.httrack.com/).