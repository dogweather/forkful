---
title:                "HTTP-pyynnön lähettäminen"
aliases:
- fi/cpp/sending-an-http-request.md
date:                  2024-01-20T17:59:04.545720-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä ja miksi?
HTTP-pyynnön lähettäminen tarkoittaa pyynnön esittämistä verkkopalvelimelle. Ohjelmoijat tekevät tämän hankkiakseen tietoa tai toimittaakseen dataa.

## How to: - Kuinka tehdä:
C++ ei suoraan tue HTTP-pyyntöjä standardikirjastossaan, mutta kirjastoja kuten `libcurl` tai `cpp-httplib` voidaan käyttää.

```C++
#include <iostream>
#include <httplib.h>

int main() {
    httplib::Client cli("http://example.com");
    
    auto res = cli.Get("/");
    if (res && res->status == 200) {
        std::cout << res->body << std::endl;
    } else {
        std::cerr << "Request failed with status: " << res->status << std::endl;
    }
    
    return 0;
}
```

Tämä koodi lähettää GET-pyynnön `http://example.com` -palvelimelle ja tulostaa vastauksen.

## Deep Dive - Syväsukellus:
HTTP on perustettu 1990-luvulla ja on internetin tiedonsiirron perusta. C++:ssa ei ole sisäänrakennettua HTTP-tukea, koska se on yleiskäyttöinen kieli eikä keskity verkkotoiminnallisuuksiin. Vaihtoehtoja `libcurl` ja `cpp-httplib` kirjastoille ovat muun muassa `Boost.Beast` ja `Poco`. Näitä kirjastoja käyttämällä voidaan hallita matalan tason verkkoyhteyksiä tai rakentaa omia HTTP-pyynnön käsittelijöitä.

## See Also - Katso myös:
- `libcurl`: https://curl.se/libcurl/
- `cpp-httplib`: https://github.com/yhirose/cpp-httplib
- `Boost.Beast`: https://www.boost.org/doc/libs/1_75_0/libs/beast/doc/html/index.html
- `Poco`: https://pocoproject.org/
