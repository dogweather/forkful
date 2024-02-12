---
title:                "Een webpagina downloaden"
aliases: - /nl/cpp/downloading-a-web-page.md
date:                  2024-01-28T21:59:19.028817-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een webpagina downloaden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/cpp/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een webpagina downloaden betekent simpelweg de inhoud ervan ophalen, gewoonlijk in HTML-formaat, om lokaal te bekijken of te verwerken. Programmeurs downloaden webpagina's om gegevens te scrapen, veranderingen te monitoren, of te integreren met webservices.

## Hoe:
In de huidige C++ versie kunt u de `CURL` bibliotheek gebruiken om webinhoud te downloaden. Hier is een basisvoorbeeld:

```cpp
#include <curl/curl.h>
#include <iostream>
#include <string>

static size_t writeCallback(void* contents, size_t size, size_t nmemb, void* userp){
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    Als(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);

        if(res == CURLE_OK) {
            std::cout << readBuffer << std::endl;
        }
        else {
            std::cerr << "CURL Error: " << curl_easy_strerror(res) << std::endl;
        }
    }

    return 0;
}
```

Voorbeelduitvoer:

```html
<!doctype html>
<html>
<head>
    <title>Voorbeeld Domein</title>
    ...
</head>
<body>
    <div>
        <h1>Voorbeeld Domein</h1>
        <p>Dit domein is voor gebruik in illustratieve voorbeelden in documenten. U mag dit domein ...</p>
    </div>
</body>
</html>
```

## Diepgaande Verkenning
Oorspronkelijk was er geen standaardmanier om webpagina's te downloaden met alleen C++. Programmeurs gebruikten platformspecifieke oplossingen of verschillende externe bibliotheken. Nu is `libcurl` een breed ondersteunde en veelzijdige bibliotheek voor het overbrengen van gegevens met URL's. Gecompileerd en gelinkt met uw C++ code, is curl een voor de hand liggend hulpmiddel.

Alternatieven voor libcurl zijn Poco's HTTPClientSession en C++ Rest SDK (ook bekend als Casablanca). Terwijl libcurl C-gebaseerd is en ongeveer zo laagdrempelig als je comfortabel kunt gaan wat betreft HTTP-verzoeken, bieden Poco en Casablanca meer idiomatische C++ interfaces die sommigen misschien verkiezen.

Achter de schermen, wanneer u een webpagina downloadt, treedt het HTTP-protocol in actie. Een GET-verzoek wordt naar de server gezonden, en als alles goed gaat, reageert de server met de inhoud ingepakt in een HTTP-antwoord.

## Zie Ook
- [officiële site van libcurl](https://curl.se/libcurl/)
- [C++ Rest SDK GitHub Repo](https://github.com/microsoft/cpprestsdk)
- [Poco Project](https://pocoproject.org/)
- [HTTP op Wikipedia](https://nl.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
