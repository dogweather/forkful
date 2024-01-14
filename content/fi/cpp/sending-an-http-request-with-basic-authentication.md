---
title:                "C++: Http-pyynnön lähettäminen perusautentikoinnilla"
simple_title:         "Http-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan ensimmäistä C++ ohjelmointi blogitekstiämme suomenkieliselle yleisölle! Tässä artikkelissa käsittelemme HTTP-pyyntöjen lähettämistä käyttäen perusautentikaatiota ja selvitämme, miksi tämä voi olla hyödyllistä ohjelmoinnissa.

## Kuinka

Voit lähettää HTTP-pyyntöjä käyttämällä perusautentikaatiota hyödyntämällä C++ ohjelmointikieltä ja sen kirjastoja kuten [cpr](https://github.com/whoshuu/cpr). Ensimmäiseksi täytyy lisätä kirjasto projektisi riippuvuuksiin ja määrittää tarvittavat muuttujat ja parametrit, kuten URL-osoite ja käyttäjänimi sekä salasana.

Alla olevassa esimerkissä luodaan yksinkertainen GET-pyyntö osoitteeseen "https://example.com" käyttäen perusautentikaatiota. `r.status_code` tulostaa vastauksen HTTP status-koodin ja `r.text` näyttää vastauksen sisällön.

```C++
#include <cpr/cpr.h>

int main(int argc, char** argv) {
    cpr::Response r = cpr::Get(cpr::Url{"https://example.com"}, 
                            cpr::Authentication{"username", "password"});
    std::cout << "Status code: " << r.status_code << std::endl;
    std::cout << "Response: " << r.text << std::endl;
    return 0;
}
```

Ohjelman suorittamisen jälkeen tulostuu seuraava:

```
Status code: 200
Response: Hello World!
```

Käyttäessäsi `cpr::Authentication` rakennetta, muista että voit myös määrittää käyttäjänimen ja salasanan sijaan vain yhden `cpr::Authentication{"credentials"}` parametrin, jos olet esimerkiksi käyttänyt samaa alaviivaa toisena parametrina URL-osoitteessa.

## Syvemmälle

Voit lähettää HTTP-pyyntöjä myös ilman perusautentikaatiota, mutta tämä voi johtaa tietoturvariskeihin. Perusautentikaatiossa käyttäjänimi ja salasana lähetetään pyynnön mukana "Authorization" otsakkeessa ja salasana on merkitty Base64-koodattuna. Huomaa, että tämä ei kuitenkaan tarjoa täytäntöönpanon turvaa ja on siksi turvallisempi käyttää muita autentikaatiomenetelmiä, kuten OAuthia.

## Katso myös

- [cpr Githubiin](https://github.com/whoshuu/cpr)
- [C++ opetusohjelmat](https://www.learncpp.com/)
- [HTTP-perusautentikaatio](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)