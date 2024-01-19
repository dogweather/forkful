---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Ladattavat verkkosivut tarkoittavat digitalisten tietojen saamista verkkosivulta paikalliselle laitteelle. Ohjelmoijat tekevät tämän usein tietojen keräämiseksi tai offline-työskentelyä varten.

## Kuinka:

Alla on esimerkki C++ koodista, joka lataa verkkosivun käyttäen `cURL` kirjastoa:

```C++
#include <curl/curl.h>
#include <string>

// Tämä funktio käytetään vastauksen käsittelyyn
size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* userp) {
    userp->append((char*)contents, size * nmemb);
    return size * nmemb;
}

std::string downloadWebpage(std::string url) {
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
    }
    return readBuffer;
}
```

Esimerkin tulostus:
```C++
int main() {
    std::string content = downloadWebpage("https://www.example.com/");
    std::cout << content;
    return 0;
}
```
Tämä antaa verkkosivun HTML-koodin.

## Syvällinen sukellus:

Historiallisessa kontekstissa verkkosivujen lataaminen on ollut olennainen tapa tiedon saamiseksi verkon läpi jo vuodesta 1991 lähtien, jolloin WWW lanseerattiin. 

Vaihtoehtona voit käyttää muita kirjastoja, kuten 'Boost.Asio', joka tukee matalamman tason verkkotoimintoja. 

Riippuen sovelluksesi tarpeista, saatat haluta tarkistaa myös verkkosivun palauttamien HTTP-otsikoiden tietoja. Tämä voi tarkoittaa esimerkiksi toimintojen lisäämistä `cURL` koodiisi `curl_easy_getinfo` funktion avulla.

## Katso myös:

Lisätietoa saa alla olevien linkkien kautta:

1. cURL:n virallisella sivustolla: [https://curl.haxx.se/libcurl/c/](https://curl.haxx.se/libcurl/c/)
2. 'Boost.Asio' kirjasto: [https://www.boost.org/doc/libs/1_76_0/doc/html/boost_asio.html](https://www.boost.org/doc/libs/1_76_0/doc/html/boost_asio.html)