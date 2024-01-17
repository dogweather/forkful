---
title:                "Pobieranie strony internetowej"
html_title:           "C++: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Cześć Czytelnicy!

Jesteś programistą i zastanawiasz się, co to znaczy "pobieranie strony internetowej" i po co programiści to robią? W tym artykule postaram się wyjaśnić te zagadnienia w bardzo prosty i bezpośredni sposób.

## Co i dlaczego?

Pobieranie strony internetowej oznacza pobranie jej zawartości, czyli tekstu, obrazków, kodu itp. z internetu na nasz komputer. Programiści często pobierają strony internetowe, aby przetwarzać zawartość w celu wyświetlenia jej użytkownikom lub wykorzystania w swoich aplikacjach.

## Jak to zrobić?

```C++
#include <iostream>
#include <curl/curl.h>

using namespace std;

long downloadPage(string url, string filename) {
  CURL *curl;
  FILE *pagefile;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_ALL);
  curl = curl_easy_init();

  if (curl) {
    pagefile = fopen(filename.c_str(), "wb");
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, pagefile);
    res = curl_easy_perform(curl);
    curl_easy_cleanup(curl);
    fclose(pagefile);
  }

  curl_global_cleanup();
  return 0;
}

int main() {
  downloadPage("https://www.google.com", "google.html");
  return 0;
}

```

Uruchamiając ten kod, pobierzemy stronę "https://www.google.com" i zapiszemy ją w pliku "google.html" w bieżącym katalogu. Proste, prawda?

## Rzuć okiem na kulisy

Pobieranie stron jest nieodłączną częścią przeglądania internetu i jest wykorzystywane przez wiele aplikacji. Często programiści korzystają z biblioteki CURL, ale istnieją też inne rozwiązania, takie jak biblioteka HTTPClient obsługująca protokół HTTP oraz framework Scrapy napisany w Pythonie.

Korzystając z CURL, możemy manipulować za pomocą wielu opcji, takich jak ustawianie nagłówków, przekierowania, autoryzacji itp. Ponadto, możemy także łatwo pobierać konkretne elementy strony, np. tekst, obrazki czy linki.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o pobieraniu stron internetowych przy użyciu biblioteki CURL, polecam przeczytać dokumentację na stronie [Oficjalna strona CURL](https://curl.se/docs/). Możesz także sprawdzić inne rozwiązania, takie jak [HTTPClient](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest), [Scrapy](https://scrapy.org/) i [urllib](https://docs.python.org/3/library/urllib.html).

Dzięki za przeczytanie tego artykułu. Mam nadzieję, że pomógł Ci zrozumieć, co to jest pobieranie stron internetowych i dlaczego jest to ważne dla programistów. Do zobaczenia!