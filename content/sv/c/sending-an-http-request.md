---
title:                "Skicka en http-förfrågan"
date:                  2024-01-20T17:59:20.183840-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en http-förfrågan"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan innebär att be en server om data eller att utföra en åtgärd. Programmerare gör detta för att interagera med webbtjänster, hämta information eller skicka data.

## Hur man gör:
```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
  CURL *curl = curl_easy_init();
  if(curl) {
    CURLcode res;
    curl_easy_setopt(curl, CURLOPT_URL, "http://httpbin.org/get");
    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    curl_easy_cleanup(curl);
  }
  return 0;
}
```
Exempelutdata:
```
{
  "args": {}, 
  "headers": {
    "Accept": "*/*", 
    "Host": "httpbin.org", 
    "User-Agent": "curl/7.64.1"
  }, 
  "origin": "xxx.xxx.xxx.xxx", 
  "url": "https://httpbin.org/get"
}
```

## Djupdykning:
Att skicka HTTP-förfrågningar i C är inte inbyggt. Bibliotek som libcurl skapades för att förenkla uppgiften. Libcurl har stöd för flera protokoll utöver HTTP och HTTPS, från FTP till SMTP. Alternativ till libcurl inkluderar programmering på lägre nivå med socket-API:er, men det är mer krävande. Implementeringen ovan använder libcurl's enkla API för att göra en GET-förfrågan, som är grundläggande men tillräcklig för många ändamål.

## Se Också:
- [libcurl C API Documentation](https://curl.haxx.se/libcurl/c/)
- [HTTPbin](http://httpbin.org/) för att testa HTTP-förfrågningar.
- [RFC 7230](https://tools.ietf.org/html/rfc7230), HTTP/1.1: Message Syntax and Routing, för att förstå HTTP på en djupare nivå.