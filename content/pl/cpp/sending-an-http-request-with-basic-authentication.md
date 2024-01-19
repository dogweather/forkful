---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem to nasz sposób na komunikację z serwerem, przekazując przy tym potrzebne nam dane. Programiści robią to, aby bezpiecznie przesyłać niezbędne informacje, takie jak login i hasło, do serwisów internetowych.

## Jak to zrobić:

Możemy to zrealizować w C++ za pomocą biblioteki libcurl. Przykładowy kod poniżej pokazuje, jak to można zrobić.

```C++
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;
 
    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();

    if (curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");

        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
          fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

        /* always cleanup */ 
        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();

    return 0;
}
```

Jeśli wszystko przebiegło poprawnie, nie otrzymasz żadnego komunikatu. W przeciwnym razie zostanie wyświetlony komunikat o błędzie.

## Rozwinięcie tematu

- Kontekst historyczny: Podstawowe uwierzytelnianie to najstarsza metoda uwierzytelniania w HTTP. Powstała jeszcze w czasach, gdy bezpieczeństwo nie było priorytetem.

- Alternatywa: Jest wiele alternatyw dla podstawowego uwierzytelniania, takich jak uwierzytelnianie Digest czy tokeny JWT. Te metody są bardziej zabezpieczone, ale również bardziej skomplikowane do implementacji.

- Szczegół implementacji: Uwierzytelnianie odbywa się przez dodanie nagłówka "Authorization" do żądania HTTP. W przypadku podstawowego uwierzytelniania, nagłówek ten zawiera słowo "Basic" poprzedzające zakodowane w base64 dane uwierzytelniające, zwykle login i hasło.

## Zobacz także:

- Dokumentacja libcurl: [https://curl.se/libcurl/c/](https://curl.se/libcurl/c/)
- HTTP Authentication: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- Base64 Encoding: [https://en.wikipedia.org/wiki/Base64](https://en.wikipedia.org/wiki/Base64)