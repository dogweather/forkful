---
title:                "Sending et http-forespørsel med grunnleggende autentisering"
html_title:           "C++: Sending et http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending et http-forespørsel med grunnleggende autentisering"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du ønsker å kommunisere med en server som krever autentisering, er det nødvendig å sende en HTTP forespørsel med grunnleggende autentisering. Dette er en vanlig måte å sikre at bare autoriserte brukere får tilgang til serveren.

## Hvordan

```C++
#include <iostream>
#include <curl/curl.h>

// Funksjon for å håndtere HTTP respons
static size_t httpResponseHandler(char* data, size_t size, size_t nmemb, void* userdata)
{
    // Skriv ut HTTP responsen
    std::cout << "HTTP respons: " << data << std::endl;
    return size * nmemb;
}

int main()
{
    // Sett opp curl håndterer
    CURL* curl = curl_easy_init();

    // Definer URL
    std::string url = "https://example.com/api";

    // Sett opp HTTP forespørsel med autentisering
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERNAME, "brukernavn");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "passord");

    // Sett opp håndterer for HTTP respons
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, httpResponseHandler);

    // Send HTTP forespørsel
    CURLcode result = curl_easy_perform(curl);

    // Sjekk for eventuelle feil
    if (result != CURLE_OK)
    {
        std::cout << "Feil: " << curl_easy_strerror(result) << std::endl;
    }

    // Rydd opp
    curl_easy_cleanup(curl);

    return 0;
}
```

Output:
```
HTTP respons: HTTP/1.1 200 OK
Content-Type: text/html;charset=utf-8
Content-Length: 1462

<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Eksempel</title>
</head>
<body>
    <h1>Velkommen til eksemplet!</h1>
    <p>Du har fått tilgang til denne siden med grunnleggende autentisering.</p>
</body>
</html>

```

## Deep Dive

Når du sender en HTTP forespørsel med grunnleggende autentisering, inkluderer du brukernavn og passord som en del av headeren i forespørselen. Dette kan potensielt være usikkert, siden passordet vil være synlig i koden din. Derfor bør du vurdere å bruke andre autentiseringsmetoder når det er mulig.

## Se også

- [HTTP Basic Authentication - Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [libcurl - Basic Authentication](https://curl.se/libcurl/c/CURLOPT_HTTPAUTH.html)