---
title:                "Wysyłanie żądania http"
html_title:           "C++: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robimy?
Wysyłanie żądania HTTP to sposób na komunikację z serwerem internetowym w celu pobrania informacji. Programiści wykorzystują je, aby pobierać dane, takie jak tekst, obrazy czy pliki, z innych stron internetowych lub aplikacji.

## Jak to zrobić:
Przykładowym sposobem na wysłanie żądania HTTP w C++ jest użycie biblioteki "libcurl". Poniższy kod pokaże, jak pobrać zawartość strony internetowej i wyświetli ją w konsoli.

```C++
#include <iostream>
#include <curl/curl.h>

int main() {
  CURL *curl;
  CURLcode res;
  curl = curl_easy_init();
  
  if(curl) {
    // ustawia URL, z którego chcemy pobrać dane
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
    
    // ustawia funkcję do zapisywania danych z serwera
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, &writeCallback);
    
    // wykonuje żądanie
    res = curl_easy_perform(curl);
    
    // sprawdza kod odpowiedzi z serwera
    long response_code;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
    
    // jeśli kod jest 200, to żądanie powiodło się
    if(response_code == 200) {
      std::cout << "Pomyślnie pobrano zawartość strony!" << std::endl;
    }
    
    // zamyka połączenie z serwerem i czyszczenie pamięci
    curl_easy_cleanup(curl);
  }
  
  return 0;
}

// funkcja do zapisywania danych z serwera
size_t writeCallback(char *buf, size_t size, size_t nmemb, void *userdata) {
  for(int i = 0; i < size*nmemb; i++) {
    std::cout << buf[i];
  }
  
  return size*nmemb;
}
```

Wynik działania programu w konsoli powinien wyglądać mniej więcej tak:

```
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title>
  <meta charset="utf-8" />
  <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <style type="text/css">
    body {
      background-color: #f0f0f2;
      margin: 0;
      padding: 0;
      font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
      
    }
    #content {
      display: table;
      width: 100%;
      height: 100%;
    }
    .center {
      text-align: center;
    }
    #content .center {
      display: table-cell;
      vertical-align: middle;
    }
    .text {
      text-align: center;
      display: inline-block;
      max-width: 80%;
      border-radius: 10px;
      padding: 10px;
      background-color: #fff;
      font-size: 24px;
    }
  </style>  
</head>

<body>
  <div id="content">
    <div class="center">
      <div>
        <img src="https://www.example.com/images/logo.png" alt="Logo">
      </div>
      <p class="text">Hello World!</p>
    </div>
  </div>
</body>
</html>
```

## Głębsza analiza:
Wysyłanie żądań HTTP jest nieodłączną częścią internetu i jest wykorzystywane przez różne aplikacje, od przeglądarek internetowych po programy komunikacyjne. Istnieje wiele bibliotek dostępnych w języku C++, dzięki którym można łatwo wysyłać żądania HTTP, takie jak "libcurl" czy "cpp-netlib".

Alternatywą dla wysyłania żądań HTTP może być korzystanie z protokołu FTP, jednak jest on mniej popularny i wykorzystywany głównie do przesyłania plików.

Podczas implementacji wysyłania żądań HTTP ważne jest uważne sprawdzanie kodów odpowiedzi z serwera, aby upewnić się, że żądanie zostało wykonane pomyślnie. Ważne jest również ustawienie odpowiednich nagłówków, aby poprawnie szczegółowo określić żądanie.

## Zobacz też:
- [libcurl](https://curl.se/libcurl/)
- [cpp-netlib](http://cpp-netlib.org/)
- [HTTP na Wikipedii](https://pl.wikipedia.org/wiki/Hypertext_Transfer_Protocol)