---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Sender en HTTP-forespørsel med C

## Hva & Hvorfor?
Å sende en HTTP-forespørsel er når en klient ber en server om data eller instrukser. Programmere gjør dette for å hente eller sende informasjon over Internett.

## Hvordan:
Her kommer noen raske eksempler på hvordan man kan sende en HTTP GET forespørsel ved å bruke C og libcurl biblioteket.

Installer først libcurl hvis du ikke allerede har det.

Linux:
```C
sudo apt-get install libcurl4-openssl-dev
```

macOS:
```C
brew install curl
```

Her er koden:
```C
#include <stdio.h>
#include <curl/curl.h>
 
int main(void) {
  CURL *curl;
  CURLcode res;
 
  curl_global_init(CURL_GLOBAL_DEFAULT);
 
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
 
    res = curl_easy_perform(curl);

    if(res != CURLE_OK) 
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
 
    curl_easy_cleanup(curl);
  }
 
  curl_global_cleanup();
 
  return 0;
}
```

## Dypdykk
Bruken av HTTP-forespørsler strekker seg tilbake til tidlig utvikling av internett selv. Tidligere ble lignende interaksjoner oppnådd gjennom telnet økter eller lignende protokoller.

Som alternativer til HTTP-forespørsler, kan utviklere bruke WebSockets for en konstant åpen forbindelse mellom klient og server, eller nyere teknologi som HTTP/2 eller HTTP/3, som gir forbedret ytelse.

Det er mange detaljer bak implementering av en HTTP-forespørsel. Det er forskjellige metoder som GET, POST, DELETE etc., samt bruk av forskjellige header felt for å styre hvordan innholdet blir behandlet av serveren og klienten.

## Se Også

- [libcurl Dokumentasjon](https://curl.haxx.se/libcurl/c/)
- [HTTP/2 Explained](https://http2.github.io/)
- [HTTP/3 Explained](https://http3-explained.haxx.se/)
- [WebSockets](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API)