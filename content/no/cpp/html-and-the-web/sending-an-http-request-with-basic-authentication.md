---
date: 2024-01-20 18:01:29.225274-07:00
description: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering betyr\
  \ at du inkluderer brukernavn og passord for \xE5 f\xE5 tilgang til en beskyttet\
  \ ressurs p\xE5 et\u2026"
lastmod: '2024-03-13T22:44:41.099647-06:00'
model: gpt-4-1106-preview
summary: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering betyr\
  \ at du inkluderer brukernavn og passord for \xE5 f\xE5 tilgang til en beskyttet\
  \ ressurs p\xE5 et\u2026"
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering betyr at du inkluderer brukernavn og passord for å få tilgang til en beskyttet ressurs på et nettsted. Programmerere gjør dette for å automatisere tilgang til tjenerressurser som krever godkjenning.

## Slik gjør du:
For å sende en HTTP-forespørsel med grunnleggende autentisering i C++, trenger du ofte et nettverksbibliotek som libcurl. Her er et enkelt eksempel:

```C++
#include <curl/curl.h>
#include <iostream>
#include <string>

int main() {
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);
  curl = curl_easy_init();
  if(curl) {
    // Set URL
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/data");

    // Set basic authentication credentials
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERPWD, "user:password");

    // Perform the request
    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      std::cerr << "curl_easy_perform() failed: " << curl_easy_strerror(res) << std::endl;
    
    // Clean up
    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();

  return 0;
}
```

Ovennevnte vil sende en GET-forespørsel til 'http://example.com/data' med brukernavnet 'user' og passordet 'password'.

## Dykket ned:
Grunnleggende autentisering er en enkel og rett fram autentiseringsmetode der brukernavn og passord blir kodet med Base64 og sendt i 'Authorization'-headeren. Selv om denne metoden har vært brukt siden tidlig i HTTP-historien, anses den for å være usikker over åpne nettverk uten SSL/TLS. Enghend viser også til andre teknikker som OAuth, som gir en sikrere måte for autentisering og tillatelser.

Det er viktig å merke seg at bruken av libcurl i C++ prosjekter er populær på grunn av dens allsidighet og kompatibilitet. Eksemplet ovenfor er bare en av mange måter å sende en HTTP-forespørsel på; andre biblioteker som `cpprestsdk` eller `Beast` (en del av Boost-bibliotekene) kan også være aktuelle alternativer.

Når det kommer til implementasjon, pass på å håndtere curl initialiseringer og rydde opp ordentlig for å unngå minnelekkasjer eller andre ressurshåndteringsproblemer. Utover selve HTTP-forespørselen, vil du sannsynligvis trenge å håndtere HTTP-responsen - dette eksemplet dekker ikke responsbehandling.

## Se også:
- cURL offisielle dokumentasjon: https://curl.se/libcurl/c/libcurl.html
- C++ REST SDK ('cpprestsdk'): https://github.com/microsoft/cpprestsdk
- Boost.Asio og Beast introduksjon: https://www.boost.org/doc/libs/1_75_0/libs/beast/doc/html/index.html
- HTTP Basic Access Authentication på MDN: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
