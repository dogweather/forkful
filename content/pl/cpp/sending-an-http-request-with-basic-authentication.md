---
title:                "Wysłanie żądania http z podstawową autoryzacją"
html_title:           "C++: Wysłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysłanie żądania http z podstawową autoryzacją"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP z podstawową autoryzacją jest sposobem na bezpieczne przesyłanie informacji między aplikacjami. Programiści często używają tego sposobu, aby upewnić się, że tylko uprawnione osoby mają dostęp do żądanych danych.

## Jak to zrobić:
Przykładowe kody i wyniki można znaleźć poniżej:

### Inicjalizacja zmiennych:
```
std::string username = "user"
std::string password = "password"
```

### Wysyłanie żądania HTTP z podstawową autoryzacją:
```
// Tworzenie nagłówka autoryzacyjnego z kodowaniem Base64 
std::string credentials = username + ":" + password;
std::string encodedCredentials = base64_encode(credentials);

// Tworzenie nagłówka Autoryzacji 
std::string authHeader = "Authorization: Basic " + encodedCredentials;

// Ustawienie uchwytu do adresu URL 
std::string url = "https://example.com/api/data";
CURL *handle = curl_easy_init();
if(handle) {
  // Dodawanie nagłówka Autoryzacji do żądania 
  struct curl_slist *headers = NULL;
  headers = curl_slist_append(headers, authHeader.c_str());
  curl_easy_setopt(handle, CURLOPT_HTTPHEADER, headers);

  // Wykonanie żądania HTTP 
  curl_easy_setopt(handle, CURLOPT_URL, url.c_str());
  curl_easy_perform(handle);

  // Zwalnianie pamięci i usuwanie nagłówka 
  curl_easy_cleanup(handle);
  curl_slist_free_all(headers);
}
```

### Przykładowy wynik:
```
<HTTP/1.1 200 OK
<Content-Type: application/json
<Content-Length: 37

<{"message": "Dane zostały pomyślnie pobrane"}
```

## Głębsze wgląd:
### Kontekst historyczny:
HTTP Basic Authentication zostało wprowadzone w 1996 roku i od tego czasu jest szeroko stosowane w celu zabezpieczenia komunikacji między aplikacjami.

### Alternatywy:
Innym sposobem na bezpieczne przesyłanie informacji są protokoły takie jak HTTPS lub wykorzystanie autoryzacji OAuth.

### Szczegóły implementacji:
Podstawowa autoryzacja polega na kodowaniu danych autoryzacyjnych za pomocą kodowania Base64. Użytkownik i hasło są konkatenowane i przypisane do nagłówka Authorization w formacie "Użytkownik:Hasło", a następnie zakodowane przy użyciu Base64.

## Zobacz także:
- [Curl – Oficjalna strona](https://curl.haxx.se/)
- [HTTP Basic Authentication na Wiki](https://en.wikipedia.org/wiki/Basic_access_authentication)