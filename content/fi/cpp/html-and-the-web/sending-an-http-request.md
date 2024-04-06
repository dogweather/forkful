---
date: 2024-01-20 17:59:04.545720-07:00
description: "How to: - Kuinka tehd\xE4: C++ ei suoraan tue HTTP-pyynt\xF6j\xE4 standardikirjastossaan,\
  \ mutta kirjastoja kuten `libcurl` tai `cpp-httplib` voidaan k\xE4ytt\xE4\xE4."
lastmod: '2024-04-05T21:53:58.439938-06:00'
model: gpt-4-1106-preview
summary: ''
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
weight: 44
---

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
