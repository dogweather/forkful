---
title:                "C: Att skicka en http-begäran med grundläggande autentisering"
simple_title:         "Att skicka en http-begäran med grundläggande autentisering"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

HTTP-anrop med grundläggande autentisering är en viktig del av webbutveckling och tillåter användare att säkert skicka och ta emot data mellan klienter och servrar. Det är också ett viktigt steg för att skydda känslig information såsom lösenord och personuppgifter.

## Hur man gör

För att skicka ett HTTP-anrop med grundläggande autentisering i C-programmering, behöver vi först inkludera nödvändiga bibliotek. Sedan behöver vi ange URL-adressen till den server vi ska skicka anropet till samt de autentiseringsuppgifter som behövs.

Exempel:

```C
#include <stdio.h> 
#include <curl/curl.h>

int main(void) {

  CURL *curl;
  CURLcode res;
  
  // Ange URL-adressen till servern
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "www.example.com");
    // Ange autentiseringsuppgifter
    curl_easy_setopt(curl, CURLOPT_USERPWD, "användarnamn:lösenord");
    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "Fel vid anrop av HTTP: %s\n",
              curl_easy_strerror(res));
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Exempel på utdata:

```
HTTP/1.1 200 OK
Date: Fri, 01 Jan 2021 12:00:00 GMT
Content-Type: text/html; charset=UTF-8
Content-Length: 306

<html>
<head>
<title>Välkommen</title>
</head>
<body>
<h1>Välkommen till vår webbplats</h1>
<p>Loggade in som: användarnamn</p>
</body>
</html>
```

## Djupdykning

Grundläggande autentisering sker genom att skicka autentiseringsuppgifter i det första HTTP-anropet. Servern svarar sedan med en HTTP-statuskod 401 (unauthorized) om autentiseringen misslyckas eller 200 (OK) om det är korrekt. Det är viktigt att notera att autentiseringsuppgifterna skickas i klartext, vilket innebär att de kan läsas av obehöriga om de snappar upp anropet. Därför är det viktigt att använda HTTPS för att säkert kryptera informationen.

## Se också

- [C-programmering: Att skicka HTTP-anrop](https://www.w3schools.com/lib/w3curl_get.asp)
- [Libcurl biblioteket](https://curl.se/libcurl/)
- [HTTP-autentisering](https://en.wikipedia.org/wiki/Basic_access_authentication)