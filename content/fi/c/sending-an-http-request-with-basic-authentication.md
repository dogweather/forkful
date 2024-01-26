---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
date:                  2024-01-20T18:00:50.745141-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
HTTP-pyyntöjen lähettäminen perusautentikaatiolla tarkoittaa palvelimille lähetettävää viestiä, jossa on käyttäjänimi ja salasana koodattuna oikeaan muotoon. Ohjelmoijat käyttävät sitä päästäkseen käsiksi suojattuihin resursseihin.

## How to: (Kuinka tehdä:)
```C
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://yourserver.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, "user:password");

        CURLcode res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        }
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
Sample output:
```
curl_easy_perform() failed: Couldn't connect to server
```

## Deep Dive (Lähempi tarkastelu)
Perusautentikaatio (Basic Authentication) on HTTP-protokollan yksinkertainen autentikaatiomekanismi. Se alkoi yleistyä alun perin 1990-luvulla. Autentikaatio on heikkouden takia nykyään harvemmin käytössä turvallisempien vaihtoehtojen, kuten OAuthin, tieltä. C-kielessä HTTP-pyyntöjä perusautentikaatiolla voidaan lähettää libcurl-kirjaston avulla. Tämä kirjasto hoidetaan monenlaisten protokollien siirtoihin, ei vain HTTP:hen.

## See Also (Katso myös)
- [libcurl Tutorial](https://curl.se/libcurl/c/libcurl-tutorial.html)
- [HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Basic authentication scheme](https://datatracker.ietf.org/doc/html/rfc7617)
