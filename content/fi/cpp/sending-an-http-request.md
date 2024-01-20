---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTTP-pyynnön lähettäminen tarkoittaa palvelimen kanssa kommunikoinnin aloittamista verkon yli HTTP-protokollaa käyttäen. Ohjelmoijat tekevät tämän hakeakseen tai lähettääkseen dataa.

## Näin teet:

Katsotaan esimerkkiä käyttämällä `C++17:n` `CPR` -kirjastoa.
```C++
#include <cpr/cpr.h>

int main() {
  cpr::Response r = cpr::Get(cpr::Url{"http://www.httpbin.org/get"},
                            cpr::Parameters{{"key", "value"}});

  std::cout << r.text << std::endl; 
  return 0;
}
```

Odotettu tuloste on seuraava:

```json
{
  "args": {
    "key": "value"
  },
  "headers": {
    "Accept": "*/*",
    "Host": "www.httpbin.org"
  },
  "origin": "YOUR_IP_ADDRESS",
  "url": "http://www.httpbin.org/get?key=value"
}
```

## Deep Dive:

Lähettäessäsi HTTP-pyyntöä, olet itse asiassa seuraamassa käytäntöä, joka on ollut olemassa vuodesta 1991, kun HTTP (Hypertext Transfer Protocol) esiteltiin. 

Vaihtoehtoisesti voit käyttää kirjastoa ASUSU. Se tarjoaa enemmän joustavuutta ja se on hyvä vaihtoehto pitkäaikaisiin projekteihin.

Toteutusyksityiskohdat: `C++17:n CPR` käärii `libcurl`-kirjaston, joka on tehokas ja monipuolinen tapa lähettää HTTP-pyyntöjä. Se ottaa URL:n ja määritteet nimettyjen parametrien muodossa, minkä jälkeen se lähettää pyynnön ja palauttaa vastauksen.

## Katso myös:

- [CPR GitHub](https://github.com/whoshuu/cpr)
- [HTTP](https://www.ietf.org/rfc/rfc2616.txt)
- [ASUSU](http://www.example.com)