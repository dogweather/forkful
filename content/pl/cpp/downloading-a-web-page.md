---
title:                "Ściąganie strony internetowej"
html_title:           "C++: Ściąganie strony internetowej"
simple_title:         "Ściąganie strony internetowej"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanawiałeś się kiedyś, jak pobierać strony internetowe za pomocą języka C++? Może chcesz stworzyć aplikację do wyświetlania aktualnych danych z internetu lub po prostu spróbować swoich sił w programowaniu? Niezależnie od powodu, pobieranie stron internetowych jest przydatną umiejętnością w dzisiejszym świecie technologii.

## Jak to zrobić

Do pobierania stron internetowych za pomocą C++ będziemy potrzebować dwóch głównych bibliotek: "iostream" i "curl". Pierwsza służy do wyświetlania danych na ekranie, a druga zajmuje się komunikacją z internetem.

```C++
#include <iostream>
#include "curl/curl.h"

int main() {
    // Inicjalizacja CURL
    CURL *curl = curl_easy_init();

    // Ustawienie adresu URL do pobrania
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");

    // Ustawienie funkcji do zapisu danych
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, fwrite);

    // Ustawienie pliku docelowego dla pobranych danych
    // W tym przypadku wykorzystujemy funkcję C do tworzenia pliku
    FILE *outfile = fopen("output.html", "w");
    if (outfile == NULL) {
        std::cout << "Nie można otworzyć pliku!" << std::endl;
        return 1;
    }
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, outfile);

    // Wykonanie zapytania i pobranie danych
    CURLcode result = curl_easy_perform(curl);
    if (result != CURLE_OK) {
        std::cout << "Błąd pobierania strony!" << std::endl;
        return 1;
    }

    // Zwolnienie pamięci i zamknięcie pliku
    curl_easy_cleanup(curl);
    fclose(outfile);

    std::cout << "Strona została pomyślnie pobrana!" << std::endl;
    return 0;
}

```

Po uruchomieniu powyższego kodu, powinno zostać pobrane źródło strony https://www.example.com i zapisane do pliku "output.html" w bieżącym folderze. Możesz teraz wykorzystać te dane w dowolny sposób, np. wyświetlić je na ekranie lub przetworzyć dalej.

## Deep Dive

Biblioteka "curl" jest bardzo potężnym narzędziem do komunikacji z internetem. Oprócz prostego pobierania stron internetowych, oferuje również możliwość wysyłania zapytań HTTP, obsługi nagłówków, wyświetlania postępu pobierania i wiele innych funkcji. Warto zapoznać się z dokumentacją tej biblioteki, aby poznać wszystkie jej możliwości.

W przypadku projektów wymagających większej niezawodności i wydajności, można również rozważyć wykorzystanie biblioteki "libcurl", która jest podstawą dla "curl". Pamiętaj jednak, że ta biblioteka jest bardziej skomplikowana w użyciu i wymaga większej wiedzy na temat protokołu HTTP i komunikacji sieciowej.

## Zobacz także

* Dokumentacja biblioteki curl: https://curl.haxx.se/libcurl/ 
* Przykład wykorzystania biblioteki: http://zetcode.com/articles/libcurl/ 
* Poradnik programowania w języku C++: https://www.w3schools.com/cpp/default.asp