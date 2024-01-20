---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobieranie strony internetowej oznacza zapisywanie jej treści na dysku twardym lub w pamięci. Programiści robią to, aby analizować małe kawałki danych, zautomatyzować procesy lub tworzyć kopie zapasowe witryn.

## Jak to zrobić:
Poniżej znajduje się przykład, jak pobrać stronę internetową za pomocą biblioteki libcurl w C++.

```C++
#include <iostream>
#include <string>
#include <curl/curl.h>

static size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* userp)
{
    userp->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main(void)
{
   CURL *curl;
   CURLcode res;
   std::string readBuffer;

   curl_global_init(CURL_GLOBAL_DEFAULT);
   curl = curl_easy_init();
   
   if(curl) {
     curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
     curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
     curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
     res = curl_easy_perform(curl);
     curl_easy_cleanup(curl);

     if(CURLE_OK == res) {
        std::cout << "Web page downloaded successfully: " << readBuffer << std::endl;
     } else {
        std::cout << "Failed to download. Error: " << curl_easy_strerror(res) << std::endl;
     }

     curl_global_cleanup();
   }

   return 0;
}
```
Po uruchomieniu powyższego kodu, treść strony internetowej http://example.com zostanie wyświetlona w konsoli.

## Deep Dive
Pierwsza metoda pobierania stron internetowych powstała z wynalezieniem Internetu i była znacznie bardziej podstawowa niż metody dzisiaj. Z czasem techniki pobierania ewoluowały, oferując szybsze, bardziej wydajne i bezpieczne metody.

Alternatywą dla C++ i libcurl może być korzystanie z innych języków i bibliotek takich jak Python z BeautifulSoup lub Node.js z axios. Wybór zależy od preferencji i wymagań projektu.

Pod kątem implementacji, libcurl obsługuje wiele protokołów, takich jak HTTP, HTTPS, FTP, ale do pobierania stron internetowych najczęściej używane są HTTP i HTTPS.

## Zobacz także
1. Dokumentacja libcurl: https://curl.se/libcurl/c/
2. Przykłady pobierania stron internetowych w Pythonie: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
3. Przykłady pobierania stron internetowych w Node.js: https://github.com/axios/axios