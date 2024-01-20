---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wysyłanie żądania HTTP to sposób, w jaki twój program komunikuje się z serwerem internetowym, wymieniając dane. Programiści robią to, aby pobierać lub wysyłać dane do zewnętrznych serwerów, tworzyć interaktywne aplikacje i strony internetowe.

## Jak to zrobić:
Aby wysłać żądanie HTTP w C, możemy skorzystać z biblioteki `libcurl`. Kod może wyglądać następująco:

``` C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);
  curl = curl_easy_init();
  
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
      
    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();
  return 0;
}
```

Kod ten wysyła żądanie GET do `http://example.com` i wyświetla odpowiedź.

## Więcej szczegółów
- Kontekst historyczny: Protokół HTTP zaczęto rozwijać w 1989 roku przez Tima Bernersa-Lee w CERN. 
- Alternatywy: Oblężenie, Apache JMeter i Locust to alternatywy dla HTTP requests w C. Mogą one być szybsze i bardziej wydajne, ale są też trudniejsze do nauki.
- Szczegóły implementacyjne: Dla wielu żądań równocześnie warto używać wielowątkowości. W przypadku dużych danych warto skorzystać z asynchronicznego przetwarzania.

## Zobacz też
- Libcurl Tutorial: http://curl.haxx.se/libcurl/c/libcurl-tutorial.html
- Protokół HTTP: https://tools.ietf.org/html/rfc2616
- Curl Github: https://github.com/curl/curl
- Wielowątkowość w C: https://www.tutorialspoint.com/c_standard_library/c_function_pthread_create.htm
- Przetwarzanie asynchroniczne: https://en.wikipedia.org/wiki/Asynchronous_I/O