---
title:                "Laste ned en nettside"
html_title:           "C: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Nedlasting av en nettside handler om å få tilgang til informasjon og data som ligger på en nettside. Dette er en vanlig oppgave for programmerere, da det er nødvendig for å hente inn og behandle data fra nettsider.

# Hvordan:
```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
  // Opprette en CURL-variabel
  CURL *curl;
  // Opprette en variabel for å lagre nettadressen
  char *url = "https://example.com";
  // Initialisere CURL-variabelen
  curl = curl_easy_init();
  // Sette nettadressen
  curl_easy_setopt(curl, CURLOPT_URL, url);
  // Utføre CURL-operasjonen og lagre resultatet i en variabel
  CURLcode res = curl_easy_perform(curl);
  // Sjekke om det skjedde en feil
  if(res != CURLE_OK)
    fprintf(stderr, "curl_easy_perform() failed: %s\n",
            curl_easy_strerror(res));
  // Rense opp etter CURL
  curl_easy_cleanup(curl);
  return 0;
}
```

**Output:** Ingen synlig output, men resultatet vil være lagret i variabelen 'res'. 

# Dypdykk:
Nedlasting av nettsider har vært en vanlig oppgave for programmerere siden internettets begynnelse. Dette kan gjøres på flere måter, for eksempel ved å bruke biblioteker som CURL eller å skrive egne programmer som kommuniserer direkte med nettverket.

En viktig ting å huske på er at nedlasting av en nettside kan være ulovlig hvis man ikke har tillatelse fra eierne av nettsiden.

# Se også:
- [CURL biblioteket](https://curl.haxx.se/libcurl/)
- [Alternativer til CURL](https://alternativeto.net/software/curl/)
- [Hvordan implementere download av nettsider i C++](https://www.codeproject.com/articles/590961/how-to-implement-download-an-html-page-in-Cplusplus)