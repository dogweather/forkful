---
title:                "Last ned en nettside"
html_title:           "C++: Last ned en nettside"
simple_title:         "Last ned en nettside"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å laste ned en nettside kan være nyttig for å få tilgang til informasjon offline, eller for å jobbe med dataanalyse.

## Hvordan gjør man det
```C++
#include <iostream>
#include <curl/curl.h>

//definerer funksjonen for å laste ned nettsiden
size_t writeCallback(char* buf, size_t size, size_t nmemb, void* up)
{
    //lagrer nedlastet data i en string
    for (unsigned int c = 0; c<size*nmemb; c++)
    {
        ((std::string*)up)->push_back(buf[c]);
    }
    return size*nmemb;
}

int main()
{   
    CURL* curl; //definerer en CURL variabel
    CURLcode res; //definerer en variabel for å sjekke resultatet
        
    //initialiserer curl og sjekker om alt fungerer som det skal
    curl = curl_easy_init();
    if (curl) 
    {
        //setter URL til nettsiden du ønsker å laste ned
        curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
        
        //definerer en string for å lagre nedlastet data
        std::string response_string;
        
        //setter opp callback funksjonen for å lagre dataen
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeCallback);
        
        //setter opp hvor dataen skal lagres
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_string);
        
        //kjører selve nedlastingen
        res = curl_easy_perform(curl);
        
        //sjekker om alt gikk bra
        if (res != CURLE_OK)
            std::cout << "Feil ved lasting av nettside" << std::endl;
        else 
        {
            //printer ut nedlastet data
            std::cout << response_string << std::endl;
        }
        //rydder opp etter curl
        curl_easy_cleanup(curl);
    }    
    return 0;
}
```

## Deep Dive
For å laste ned en nettside i C++ bruker vi biblioteket libcurl, som gjør det enkelt å kommunisere med ulike protokoller som HTTP, HTTPS, FTP og mer. CURL er også kjent for sin evne til å håndtere flere samtidige nedlastinger og støtter også proxy servere og autentisering.

## Se også
- Offisiell dokumentasjon for libcurl: https://curl.se/libcurl/
- Enkel guide til libcurl: https://devblogs.microsoft.com/cppblog/using-libcurl-with-cpp-in-visual-studio/
- Eksempelkode for å laste ned en PDF fra en nettside: https://curl.se/libcurl/c/example.html