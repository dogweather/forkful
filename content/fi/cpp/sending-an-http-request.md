---
title:                "Lähettämään http-pyyntö"
html_title:           "C++: Lähettämään http-pyyntö"
simple_title:         "Lähettämään http-pyyntö"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
HTTP-pyynnön lähettäminen on tärkeä osa web-ohjelmointia. Se tarkoittaa datan tai tiedon lähettämistä yhdestä tietokoneesta (lähtee) toiselle (saaja) verkossa käyttäen HTTP-protokollaa. Tämä on tapa, jolla ohjelmoijat viestivät web-sovellusten kanssa, kuten esimerkiksi lähettävät tietoa palvelimille tai hakukoneita.

## Miten:
Koodi-C++ ohjelmoijat käyttävät bibliotekkia, kuten HttpClient, jotta he voivat helposti ja tehokkaasti lähettää HTTP-pyynnön. Alla olevassa esimerkissä näemme, kuinka voidaan lähettää GET-pyynto palvelimelle ja tulostaa vastauksen:
```C++
#include <iostream>
#include "HttpClient.h"

using namespace std;

int main()
{
    HttpClient client("www.example.com");
    // Luodaan HTTP-client ja annetaan url
    string response = client.send_request("GET", "/index.html");  // Lähetetään GET-pyynto
    cout << response << endl; // Tulostetaan palvelimen vastaus
    return 0;
}
```
**Tuloste:**
```
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
<link rel="bolaan" href="https://maxcdn.bootstrapcdn.com">
</head>
<body>
<div>
<h1>Example Domain</h1>
<p>This domain is for use in illustrative examples in documents. You may use this domain in examples without prior coordination or asking for permission.</p>
<p>More information...</p>
</div>
</body>
</html>
```

## Syvällinen sukellus:
HTTP-pyyntöjen lähettämiseen on olemassa useita eri tapoja ja työkaluja. Ennen bibliotekkien käyttöä, ohjelmoijat voivat myös luoda omia HTTP-pyyntöjä luomalla HTTP-yhteyden ja muodostamalla pyynnön manuaalisesti. Tämä voi olla hyödyllistä tarkemman kontrollin saamiseksi, mutta se voi myös olla aikaa vievää ja monimutkaista.

## Katso myös
Lisätietoja HTTP-pyyntöjen lähettämisestä löytyy esimerkiksi näistä lähteistä:
- [W3Schools - HTTP Requests](https://www.w3schools.com/tags/ref_httpmethods.asp)
- [MDN Web Docs - HTTP Request Methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [Microsoft Docs - HTTP Request Methods](https://docs.microsoft.com/en-us/dotnet/api/system.net.httpwebrequest.method?view=netcore-3.1)