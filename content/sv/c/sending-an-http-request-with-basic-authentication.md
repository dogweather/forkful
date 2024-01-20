---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-förfrågan med grundläggande autentisering innebär att man får tillstånd att kommunicera med en server genom att ange legitima inloggningsuppgifter. Utvecklare gör detta för att säkerställa att bara auktoriserade användare kan få tillgång till vissa data.

## Hur man gör:

Här är ett kodexempel i C för att skicka en HTTP-förfrågan med grundläggande autentisering med hjälp av `libcurl` biblioteket.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
CURL *curl;
CURLcode res;

curl_global_init(CURL_GLOBAL_DEFAULT);

curl = curl_easy_init();
if(curl) {
curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");

struct curl_slist *headers = NULL;
headers = curl_slist_append(headers, "Authorization: Basic dXNlcjp1c2Vy"); // "user:password" => "dXNlcjp1c2Vy"

curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

res = curl_easy_perform(curl);

if(res != CURLE_OK)
fprintf(stderr, "curl_easy_perform() failed: %s\n",
curl_easy_strerror(res));

curl_easy_cleanup(curl);
}

curl_global_cleanup();

return 0;
}
```
Exempelutdatan kan likna:
`<p>Hello, authorized user!</p>`

## Djupdykning

Att skicka HTTP-förfrågan med autentisering i form av basisk autentisering är en teknik som blivit utbrett tack vare dess enkelhet. Historiskt sett har det varit den go-to-metoden för HTTP-autentisering sedan starten av webben, och anses säkert om det kombineras med SSL/TLS.

Alternativ till grundläggande autentisering inkluderar Digest-autentisering, eller mer moderna metoder som OAuth. Valet beror på vilket säkerhetsnivå som krävs och vilka resurser som finns tillgängliga.

Implementeringsdetaljer skiljer sig något beroende på vilket bibliotek man använder. I `libcurl` till exempel, ställs `CURLOPT_HTTPHEADER` alternativet till en lista med anpassade rubriker som inkluderar "Authorization: Basic" följt av inloggningsuppgifterna i form av en base64-kodad sträng.

## Se också

- `libcurl` dokumentation: https://curl.se/libcurl/c/
- HTTP-autentisering: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Alternativa autentiseringsmetoder: https://datatracker.ietf.org/doc/html/rfc2617