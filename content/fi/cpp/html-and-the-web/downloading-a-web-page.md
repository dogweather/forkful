---
title:                "Verkkosivun lataaminen"
aliases: - /fi/cpp/downloading-a-web-page.md
date:                  2024-01-20T17:44:10.397686-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Webbisivun lataaminen tarkoittaa sen sisällön hakemista internetistä. Ohjelmoijat lataavat sivuja tietojen keräämiseen, palveluiden integrointiin ja automaatioon.

## How to: (Kuinka Tehdä: )
C++:ssa webbisivun lataaminen vaatii HTTP-pyynnön tekemistä. Käytetään `cpr`-kirjastoa, joka on C++:n wrapperi tyypillisille HTTP-pyynnöille.

```cpp
#include <cpr/cpr.h>
#include <iostream>

int main() {
    cpr::Response r = cpr::Get(cpr::Url{"http://example.com"});

    std::cout << "Statuskoodi: " << r.status_code << std::endl;
    std::cout << "Headerit:" << std::endl;
    for(auto& header : r.header) {
        std::cout << header.first << ": " << header.second << std::endl;
    }    
    std::cout << "Sivun sisältö: \n" << r.text << std::endl;

    return 0;
}
```

Esimerkkituloste:
```
Statuskoodi: 200
Headerit:
Content-Encoding: gzip
Content-Type: text/html; charset=UTF-8
...
Sivun sisältö: 
<!doctype html>
<html>
<head>
    <title>Esimerkki</title>
...
</html>
```

## Deep Dive (Sukellus Syvyyksiin)
Alkuaikoina webbisivujen lataamista varten ohjelmoijat käyttivät matalan tason verkko-ohjelmointia ja puhdasta HTTP-protokollaa. `libcurl` on vanha ja monipuolinen kirjasto tähän tarkoitukseen. Nykyisin on olemassa useita helppokäyttöisiä kirjastoja, kuten esimerkiksi `cpr`, joka tekee HTTP-pyyntöjen käsittelystä suoraviivaista.

Vaihtoehtoisesti, voitaisiin käyttää `Boost.Beast` kirjastoa, joka arvostaa suorituskykyä ja matalan tason hallintaa. Jokaisella kirjastolla on hyvät ja huonot puolensa eri käyttötilanteissa.

HTTP/HTTPS-protokollien ymmärtäminen on tärkeää, sillä pyyntöjen tekeminen ja vastausten käsittely vaativat protokollan mukaista kommunikaatiota. Tämä ymmärrys auttaa virhetilanteissa ja mahdollistaa monimutkaisempien sovellusten rakentamisen.

## See Also (Katso Myös)
- `cpr` GitHub-sivu: https://github.com/libcpr/cpr
- `libcurl`: https://curl.se/libcurl/
- `Boost.Beast`: https://www.boost.org/doc/libs/release/libs/beast/
- HTTP-protokollan dokumentaatio: https://developer.mozilla.org/en-US/docs/Web/HTTP
