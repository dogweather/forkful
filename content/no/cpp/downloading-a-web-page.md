---
title:                "Nedlasting av en nettside"
html_title:           "C++: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Hva & hvorfor?

Nedlasting av en nettside handler om å hente data fra en URL og lagre den på en lokal datamaskin. Dette gjøres ofte av utviklere for å utforske og manipulere nettstedets innhold, eller for å lage applikasjoner som samhandler med nettstedet.

Hvordan å:

```C++
#include <iostream>
#include <curl/curl.h>

int main() {
  CURL *curl;
  CURLcode res;
  std::string url = "https://www.example.com";

  curl = curl_easy_init();

  if (curl) {
    curl_easy_setopt(curl, CURLOPT_URL, url);
    // Her kan du legge til flere innstillinger som ønskes
    res = curl_easy_perform(curl);

    if (res == CURLE_OK) {
      std::cout << "Nettsiden ble lastet ned!" << std::endl;
    } else {
      std::cerr << "Feil under nedlasting: " << curl_easy_strerror(res) << std::endl;
    }

    curl_easy_cleanup(curl);
  }

  return 0;
}
```

Eksempeloutput:

```
Nettsiden ble lastet ned!
```

Dypdykk:

For å laste ned en nettside trenger man et program som støtter HTTP-protokollen. I mange tilfeller vil man bruke et tredjepartsbibliotek som cURL. Det finnes også forskjellige kommandoer og verktøy som kan gjøre det samme, som for eksempel wget eller PowerShell i Windows.

Se også:

- [cURL dokumentasjon](https://curl.haxx.se/docs/)