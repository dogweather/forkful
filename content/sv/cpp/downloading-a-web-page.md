---
title:                "Nedladdning av en webbsida"
html_title:           "C++: Nedladdning av en webbsida"
simple_title:         "Nedladdning av en webbsida"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Det kan finnas flera anledningar till att vilja ladda ner en websida. Det kan vara för att spara den offline, studera dess kod för utbildningsändamål, eller använda den som referens för ett projekt.

## Hur man gör det

Att ladda ner en websida i C++ är ganska enkelt med hjälp av biblioteket cURL. Först måste du inkludera biblioteket i din kod:

```C++
#include <curl/curl.h>
```

Sedan kan du skapa en funktion som tar in en URL som argument och använder cURL för att hämta sidans innehåll:

```C++
std::string getWebpage(std::string url) {
    CURL *curl;
    CURLcode res;
    std::string content;

    // Initiera cURL
    curl = curl_easy_init();
    if(curl) {
        // Sätt URL:en
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());

        // Lagra innehållet i en variabel
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writer);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &content);

        // Ladda ner sidan
        res = curl_easy_perform(curl);

        // Stäng cURL
        curl_easy_cleanup(curl);

        // Om allt gick bra, returnera innehållet
        if(res == CURLE_OK) {
            return content;
        }
    }
    // Om något gick fel, returnera en tom sträng
    return "";
}

// Funktionen som används för att lagra sidans innehåll
static size_t writer(char *content, size_t size, size_t nmemb, std::string *buffer) {
    size_t newLength = size*nmemb;
    buffer->append(content, newLength);
    return newLength;
}
```

För att använda funktionen, bara mata in en URL och lagra det returnerade innehållet i en variabel:

```C++
std::string webpage = getWebpage("https://www.example.com");
```

Du kan sedan använda innehållet av sidan för vad du än behöver.

## Djupdykning

Det finns mycket mer att lära sig om hur man hämtar en websida i C++, som till exempel att ange olika användaragenter, hantera omdirigeringar och implementera felhantering. Du kan också undersöka andra bibliotek, som libcurl och libwww, för att se vilket som passar dina behov bäst.

## Se också

- [cURL Library](https://curl.haxx.se/libcurl/)
- [libcurl Tutorial](https://curl.haxx.se/libcurl/c/libcurl-tutorial.html)
- [libwww Library](https://www.w3.org/Library/)