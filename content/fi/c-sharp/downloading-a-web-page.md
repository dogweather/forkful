---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Lataaminen tarkoittaa verkkosivun tallentamista omalle tietokoneelle tai laitteelle. Tätä tehdään yleensä ohjelmoidessa, jotta voitaisiin käsitellä verkkosivuston sisältöä tai käyttää sitä muissa tarkoituksissa.

## Kuinka tehdä:
Seuraavassa on esimerkkejä koodista ja tulosteista ```C# ... ``` koodilohkossa.

### Esimerkki 1:
```
using System;
using System.Net;

string url = "https://www.example.com";
using (WebClient client = new WebClient())
{
    string html = client.DownloadString(url);
    Console.WriteLine(html);
}
```
Tuloste:
```
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title>

  <meta charset="utf-8" />
  <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
</head>

<body>
<div>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
    <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
```

### Esimerkki 2:
```
using System;
using System.Net;

string url = "https://www.example.com";
using (WebClient client = new WebClient())
{
    client.DownloadFile(url, "example.html");
    Console.WriteLine("Verkkosivu ladattu onnistuneesti!");
}
```
Tuloste:
```
Verkkosivu ladattu onnistuneesti!
```

## Syventävää tietoa:
Lataaminen verkkosivuilta on ollut olennainen osa ohjelmointia alusta alkaen. Sillä on monia erilaisia käyttötarkoituksia, kuten tiedon kerääminen, web-sovellusten testaus ja verkkosisältöjen tallentaminen offline-käyttöä varten.

On olemassa myös muita tapoja ladata verkkosivuja, kuten käyttämällä HTTP-kutsuja suoraan ja käyttämällä erilaisia kirjastoja ja työkaluja. Suositeltavin tapa riippuu käyttötarkoituksesta ja omista mieltymyksistäsi.

Yllä oleva koodi esimerkki käyttää WebClient- ja DownloadString-metodeja. WebClient on osa .NET Frameworkia ja tarjoaa yksinkertaisen tavan ladata verkkosivuja. DownloadString-metodi tallentaa verkkosivun sisällön muuttujaan, jota voidaan käsitellä halutulla tavalla.

## Katso myös:
- [MSDN - WebClient Class (Englanniksi)](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=netframework-4.7.2)
- [W3Schools - C# Download File (Englanniksi)](https://www.w3schools.com/cs/cs_ref_webclient.asp)