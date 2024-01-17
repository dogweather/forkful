---
title:                "Sända ett http-begäran med grundläggande autentisering"
html_title:           "C: Sända ett http-begäran med grundläggande autentisering"
simple_title:         "Sända ett http-begäran med grundläggande autentisering"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan med grundläggande autentisering är när en programmerare autentiserar sig med ett användarnamn och lösenord för att få åtkomst till en viss webbplats eller resurs. Programmare gör detta för att säkerställa att endast auktoriserade användare kan få tillgång till resursen och för att skydda användarnas information.

## Hur man:
Det finns två sätt att skicka en HTTP-förfrågan med grundläggande autentisering i C. Det första sättet är att inkludera autentiseringsuppgifter i URL:en, till exempel "https://användarnamn:lösenord@webbplats.com".
Det andra sättet är att använda en HTTP-bibel som cURL och specificera autentiseringsuppgifterna i förfrågan. Nedan är kodexempel för båda metoderna:

```
// Metod 1: Inkludera autentiseringsuppgifter i URL:en
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://anvandarnamn:losenord@webbplats");
    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

```
// Metod 2: Använd HTTP-biblioteket cURL
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://webbplats.com/");
    curl_easy_setopt(curl, CURLOPT_USERNAME, "användarnamn");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "lösenord");
    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

### Utmatning:
Om autentiseringen lyckas kommer förfrågan att returnera den önskade sidan eller resursen. Om autentiseringen misslyckades kommer det att returneras en HTTP-felkod, som 401 (Oauktoriserad) eller 403 (Förbjuden).

## Djupdykning:
Det är viktigt att notera att grundläggande autentisering inte längre anses vara en säker autentiseringsmetod eftersom det inte krypterar användarnas uppgifter. Istället rekommenderas att använda säkrare autentiseringsmetoder som OAuth eller TLS. En annan alternativ metod för autentisering är Digest Access Authentication, som krypterar lösenordet innan det skickas över HTTP.

Det finns olika implementationer av grundläggande autentisering i C, med cURL som den mest populära. Detta är eftersom cURL redan har stöd för grundläggande autentisering inbyggt. Men det finns också andra HTTP-bibliotek som kan användas för att skicka en förfrågan med grundläggande autentisering, som libcurl och ngx_http_auth_basic_module.

## Se även:
- [RFC 7617](https://tools.ietf.org/html/rfc7617) för den officiella specifikationen av HTTP-grundläggande autentisering
- [cURL dokumentation](https://curl.haxx.se/libcurl/c/http-auth.html) för mer information om hur man använder cURL för HTTP-autentisering