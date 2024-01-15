---
title:                "Pobieranie strony internetowej"
html_title:           "C: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Przeglądanie stron internetowych jest powszechnym zajęciem w dzisiejszych czasach. Czasami chcemy zapisać sobie ważne informacje z danej witryny, a innym razem po prostu chcemy mieć dostęp do niej w trybie offline. W tym artykule dowiemy się, jak w prosty sposób pobrać stronę internetową za pomocą języka C.

## Jak to zrobić

Do pobierania stron internetowych w języku C potrzebujemy dwóch bibliotek: `stdio.h` i `curl.h`. Następnie należy utworzyć obiekt typu `FILE` za pomocą funkcji `fopen()`, aby zapisać zawartość strony do pliku. Oto prosty przykład:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  FILE *file;

  curl = curl_easy_init();
  file = fopen("strona.html", "wb");

  if(curl)
  {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com"); // tu podajemy adres strony
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL); // funkcja którą trzeba zdefiniować jeśli chcemy coś wykonać z pobraną zawartością
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, file); // wskaźnik do pliku do którego będzie zapisywana pobrana zawartość strony

    CURLcode code = curl_easy_perform(curl); // uruchamiamy zapytanie

    if(code == CURLE_OK)
    {
      printf("Strona pobrana pomyślnie!\n");
    }
    else
    {
      printf("Wystąpił błąd: %s\n", curl_easy_strerror(code));
    }

    curl_easy_cleanup(curl);
    fclose(file);
  }

  return 0;
}
```

Powyższy kod ściąga zawartość strony `https://www.example.com` i zapisuje ją do pliku `strona.html` w tym samym folderze, w którym znajduje się program. Jeśli chcesz wykorzystać pobraną zawartość w inny sposób, możesz zdefiniować własną funkcję w `CURLOPT_WRITEFUNCTION` i przekazać ją jako ostatni argument w `CULROPT_WRITEDATA`.

## Deep Dive

Funkcja `curl_easy_setopt()` pozwala nam ustawiać różne opcje dla żądania HTTP. W przykładzie wyżej wykorzystaliśmy opcje `CURLOPT_URL`, `CURLOPT_WRITEFUNCTION` i `CURLOPT_WRITEDATA`, ale istnieje wiele innych, takich jak `CURLOPT_HTTPHEADER`, `CURLOPT_SSL_VERIFYPEER`, `CURLOPT_FOLLOWLOCATION` itp. Więcej informacji na temat wszystkich dostępnych opcji można znaleźć w dokumentacji biblioteki `libcurl`.

## Zobacz też

- [Dokumentacja biblioteki libcurl](https://curl.haxx.se/libcurl/c/)
- [Oficjalna strona języka C](https://www.iso.org/standard/74528.html)
- [Przykład użycia biblioteki libcurl do pobierania plików](https://bageac.com/blog/2016/01/05/download-ftp-http-with-libcurl-in-c-using-username-and-password-and-save-files-with.html)