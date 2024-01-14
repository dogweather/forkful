---
title:                "C++: Skicka en http-begäran"
simple_title:         "Skicka en http-begäran"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför
Att skicka en HTTP-förfrågan är ett viktigt koncept inom programmering, särskilt när man utvecklar webbapplikationer. Genom att använda HTTP-förfrågningar kan vi kommunicera med externa servrar och hämta eller skicka data via internet.

## Hur man gör
Att skicka en HTTP-förfrågan i C++ är en relativt enkel process. Först behöver vi inkludera biblioteket `curl` genom att ange `#include <curl/curl.h>` i vår kod. Sedan kan vi skapa en `CURL`-objekt och ställa in önskad URL för att skicka förfrågan till. Slutligen behöver vi bara utföra `curl_easy_perform()` för att faktiskt skicka förfrågan och få ett svar från servern.

```C++
#include <curl/curl.h>

int main() {
  CURL *curl;
  CURLcode res;
  curl = curl_easy_init();

  if (curl) {
    // Ställ in önskad URL
    curl_easy_setopt(curl, CURLOPT_URL, "http://api.example.com");

    // Utför förfrågan
    res = curl_easy_perform(curl);

    // Kontrollera om förfrågan lyckades
    if (res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    // Stäng CURL-objektet
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Output:

```
<!DOCTYPE html>
<html>
  <!-- HTML-kod från servern -->
</html>
```

## Djupdykning
Det finns olika sätt att skicka en HTTP-förfrågan i C++, beroende på vilket bibliotek du väljer att använda. I exemplet ovan använde vi `libcurl`, men det finns också andra alternativ som `cpp-netlib` och `Boost.Beast`.

När du skapar en HTTP-förfrågan finns det också olika parametrar du kan ställa in, som till exempel HTTP-metod (GET, POST, PUT osv.), headers och data att skicka med förfrågan. Det är viktigt att följa HTTP-protokollet för att få korrekta svar från servern.

En HTTP-förfrågan består av en URL och en HTTP-metod, och kan också ha en body med data som skickas tillsammans med förfrågan.

## Se även
- [cpp-netlib](https://cpp-netlib.org)
- [Boost.Beast](https://www.boost.org/doc/libs/1_72_0/libs/beast/doc/html/index.html)
- [libcurl dokumentation](https://curl.haxx.se/libcurl/c/)
- [HTTP-protokollet för begynnare](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)