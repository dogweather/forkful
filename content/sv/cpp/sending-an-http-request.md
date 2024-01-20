---
title:                "Skicka en http-förfrågan"
html_title:           "Javascript: Skicka en http-förfrågan"
simple_title:         "Skicka en http-förfrågan"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-begäran innebär att man skickar en request till en server för att interagera med en webbplats eller ladda ner data. Programmerare gör detta för att hämta, sända eller uppdatera information över internet.

## Hur man gör:

Vi kommer att använda biblioteket `C++ REST SDK (Casablanca)` för detta. Installationsinstruktioner finns [här](https://github.com/Microsoft/cpprestsdk).

```C++
    #include <cpprest/http_client.h>
    #include <cpprest/filestream.h>

    using namespace utility; // Common utilities like string conversions
    using namespace web; // Common features like URIs.
    using namespace web::http; // Common HTTP functionality
    using namespace web::http::client; // HTTP client
    using namespace concurrency::streams; // Asynchronous streams

    int main(int argc, char *argv [])
    {
        http_client client(U("http://example.com"));

        http_request request(methods::GET);

        client.request(request)
        .then([](http_response response){
            return response.extract_string();
        }).then([](std::string body){
            printf("%s", body.c_str());
        }).wait();
       
        return 0;
    }
```

Kodrutan ovan skickar en GET-begäran till "http://example.com" och skriver ut responsdata som en sträng.

## Djupdykning

HTTP-begäranden har använts sedan tidigt 90-tal då HTTP/1.0-protokollet först definierades. Alternativ till `C++ REST SDK (Casablanca) `inkluderar bibliotek som `CURL` och `Boost.Asio`.

Oftast måste de HTTP-begäranden skickas asynkront för att inte blockera programflödet, vilket gör att svar kan hanteras när de kommer in istället för att vänta på att de ska komma tillbaka innan du kan fortsätta. Detta är mycket viktigt i program som är beroende av användarinteraktion, såsom webbläsare.

## Se även

Läs mer om C++ REST SDK (Casablanca) [här](https://github.com/Microsoft/cpprestsdk/wiki).
För alternativa bibliotek, titta på [CURL](https://curl.haxx.se/libcurl/c/) eller [Boost.Asio](https://think-async.com/Asio/).
För mer om HTTP-begäranden och dess historia, se [här](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview).