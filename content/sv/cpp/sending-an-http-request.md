---
title:                "Sända en http-begäran"
html_title:           "C++: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför
Vill du kunna kommunicera med webbsidor och webbservrar från ditt C++ program? Det kan du göra genom att skicka HTTP förfrågningar. Låt oss ta en titt på hur du kan göra det!

## Såhär gör du
För att skicka en HTTP förfrågan i C++ behöver du använda dig av ett externt bibliotek, som till exempel `libcurl`. Här är ett enkelt exempel som skickar en GET förfrågan till hemsidan `example.com`:

```C++
#include <iostream>
#include <curl/curl.h>

int main()
{
    // Initiera ett curl "handle"
    CURL* curl = curl_easy_init();

    // Ange URL:en som ska hämtas
    curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com");

    // Utför GET förfrågan
    CURLcode res = curl_easy_perform(curl);

    // Kolla om förfrågan lyckades
    if (res != CURLE_OK)
    {
        std::cout << "Kunde inte skicka förfrågan" << std::endl;
    }

    // Stäng ner curl
    curl_easy_cleanup(curl);

    return 0;
}
```

Om förfrågan lyckas kommer du att få hemsidans HTML-kod skriven till terminalen. Det är också möjligt att skicka andra typer av förfrågningar, som till exempel POST eller PUT, genom att använda lämpliga `curl_easy_setopt` funktioner.

## Djupdykning
Det finns många olika parametrar och inställningar som du kan använda för att skicka mer avancerade HTTP förfrågningar med `libcurl`. Du kan till exempel ange HTTP headers, hantera cookies, och skicka data som JSON eller XML. Det är också möjligt att använda olika säkerhetsinställningar, som SSL-certifikat, för att göra förfrågningar till webbsidor med HTTPS-protokoll. För att lära dig mer om alla de möjligheter som `libcurl` erbjuder, kolla in dokumentationen på deras hemsida.

## Se även
- [libcurl hemsida](https://curl.haxx.se/libcurl/)
- [C++ Programmerings Språksguide](https://www.cplusplus.com/doc/tutorial/)