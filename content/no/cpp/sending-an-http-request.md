---
title:                "C++: Å sende en http forespørsel"
simple_title:         "Å sende en http forespørsel"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sende HTTP-forespørsler er en viktig del av webutvikling og serverkommunikasjon. Det lar deg be om og motta data fra en ekstern server, som gjør det mulig å lage dynamiske og interaktive nettsteder. 

## Slik gjør du det

For å sende en HTTP-forespørsel i C++, trenger vi å bruke et bibliotek som kalles "libcurl". Dette biblioteket gir oss funksjoner og metoder for å utføre forskjellige HTTP-operasjoner, for eksempel GET-forespørsler for å hente data og POST-forespørsler for å legge inn data. 

### Eksempelkode:

```C++
#include <iostream>
#include <curl/curl.h>

using namespace std;

// Denne funksjonen vil bli kalt av libcurl når en HTTP-respons mottas
// Her kan vi behandle responsen ved å skrive den ut eller gjøre andre handlinger
static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp)
{
    ((string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main()
{
    CURL *curl;
    CURLcode res;
    string response;

    // Initialiserer curl
    curl = curl_easy_init();

    if(curl) {
        // Setter URL-en du vil sende en GET-forespørsel til
        curl_easy_setopt(curl, CURLOPT_URL, "https://api.example.com");

        // Setter WriteCallback-funksjonen vår som skal bli kalt når en respons er mottatt
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        // Setter variabelen hvor responsen skal bli lagret
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

        // Utfører forespørselen og lagrer resultatet i 'res'
        res = curl_easy_perform(curl);

        // Sjekker om forespørselen var vellykket
        if(res != CURLE_OK) {
            // Hvis det ikke var det, skriver vi ut feilmeldingen
            cout << "Forespørselen feilet: " << curl_easy_strerror(res) << endl;
        } else {
            // Hvis det var vellykket, skriver vi ut responsen som ble lagret i 'response'
            cout << response << endl;
        }

        // Lukker curl-objektet
        curl_easy_cleanup(curl);
    }

    return 0;
}
```

### Konsolloutput:

````
{"message": "Forespørselen ble utført suksessfullt!"}
````

Som du ser, bruker vi `curl`-"easy" grensesnittet for å sende en GET-forespørsel og lagre responsen i en variabel. Dette er en veldig grunnleggende implementasjon, og libcurl tilbyr mange flere funksjoner og muligheter for å tilpasse HTTP-forespørsler.

## Dykk dypere

For å dykke dypere inn i hvordan HTTP-forespørsler fungerer, er det nødvendig å forstå protokollen bak. HTTP eller "Hypertext Transfer Protocol" er den standardiserte metoden for kommunikasjon mellom klienter og servere på nettet. Det består av forskjellige metoder, som GET, POST, PUT, DELETE og mer, og hver av disse har et spesifikt formål og bruk.

Det er også viktig å forstå HTTP-responskoder som blir returnert fra serveren. Disse kodene indikerer om forespørselen var vellykket, og hvis ikke, hva som gikk galt. Noen vanlige responskoder er 200 for "OK", 404 for "Ikke funnet" og 500 for "Intern serverfeil".

Libcurl gir også muligheter for å tilpasse HTTP-forespørsler ved å legge til header-informasjon, autentisering, og mye mer. Det er definitivt verdt å utforske for å få en grundig forståelse av hvordan å sende HTTP-forespørsler i C++.

## Se også

- [libcurl dokumentasjon](https://curl.se/libcurl/c/)
- [HTTP spesifikasjonen](https://tools