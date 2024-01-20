---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego? - What & Why?

Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem to sposób na żądanie zasobów z chronionego serwera. Programiści robią to, aby uzyskać dostęp do danych, które są zabezpieczone przed ogólnym dostępem.

## Jak to zrobić - How to:

Bibliteka libcurl jest przyjazna dla C i umożliwia łatwe zrealizowanie tego zadania. Poniżej znajduje się kod, który przeczyta stronę www, podając login i hasło.

```C
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/");

    /* username i password dla autentykacji w formacie login:hasło */
    curl_easy_setopt(curl, CURLOPT_USERPWD, "user:pass");

    /* Jeżeli serwer nie potwierdzi uwierzytelnienia to żądanie nie zostanie wysłane */
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);

    res = curl_easy_perform(curl);

    /* Sprawdzenie błędów */
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

    /* Czyszczenie */
    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();

  return 0;
}
```

## Pogłębione spojrzenie - Deep Dive

Historia wysyłania żądań HTTP z podstawowym uwierzytelnieniem rozpoczęła się wraz z powstaniem protokołu HTTP. Warto mieć na uwadze, że podstawowe uwierzytelnienie jest prostym i niezbyt bezpiecznym mechanizmem, który nie szyfruje haseł. Dzisiaj często zastępuje go uwierzytelnianie typu bearer z tokenem.

Degtyarev pisał o libcurl, jak o "najbardziej znanym multiplatformowym narzędziu przesyłającym dane z użyciem URL", a jego stosowanie jest szeroko uznane wśród programistów C.

## Zobacz także - See Also

Po więcej informacji i innych przykładach skieruj się do [dokumentacji libcurl](https://curl.haxx.se/libcurl/c/). Możesz również odwiedzić [dokumentację RFC2617](https://tools.ietf.org/html/rfc2617), aby poznać szczegóły podstawowego uwierzytelnienia HTTP. Dodatkowo [ten filmik na YouTube](https://www.youtube.com/watch?v=Qt3ZsL1WfF0) dokładnie wyjaśnia, jak używać biblioteki libcurl w C.