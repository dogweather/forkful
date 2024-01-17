---
title:                "Lähettäminen http-pyyntö perusautentikoinnilla"
html_title:           "C#: Lähettäminen http-pyyntö perusautentikoinnilla"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnilla"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Miksi ja mitä?

Lähettäessä HTTP-pyyntöä, jossa on perusautentikointi, lähetetään pyyntö yhdessä käyttäjän tunnisteiden kanssa. Tämä on yleinen tapa varmistaa, että vain oikeat käyttäjät pääsevät tiettyyn resurssiin. Koodaajat tekevät tämän usein turvallisuussyistä tai saadakseen tietyn tiedon käyttöönsä.

Kuinka:
Kaikissa alla olevissa esimerkeissä oletetaan, että haluat lähettää HTTP-pyynnön käyttäen perusautentikointia.

```c#
using System.Net;

// Luodaan uusi WebRequest-olio
WebRequest request = WebRequest.Create("https://www.example.com/api");

// Asetetaan pyynnön HTTP-metodi
request.Method = "GET";

// Lisätään käyttäjän tunnistetiedot pyyntöön
string username = "käyttäjätunnus";
string password = "salasana";
request.Credentials = new NetworkCredential(username, password);

// Lähetetään pyyntö ja tallennetaan vastaus
WebResponse response = request.GetResponse();

// Tulostetaan vastauksen statuskoodi ja sisältö
Console.WriteLine("Statuskoodi: " + ((HttpWebResponse)response).StatusCode);
Console.WriteLine("Sisältö: " + response.Content);
```

Syöte:
```
Statuskoodi: 200 OK
Sisältö: <html>
<title>Esimerkki</title>
<body>
<h1>Tervetuloa</h1>
</body>
</html>
```

Deep Dive:
Historiallisesti perusautentikointi on ollut yksi yleisimmistä tavoista suojata HTTP-pyyntöjä. Siinä käyttäjän tunnisteet lähetetään suoraan pyynnössä, mikä tekee siitä haavoittuvaisen tietoturvariskeille. Nykyään yhä useammin käytetään turvallisempia vaihtoehtoja, kuten SSL-sertifikaattia tai OAuth-autentikointia.

Jos haluat lähettää HTTP-pyynnön, jossa on perusautentikointi, on tärkeää varmistaa, että käytät HTTPS-protokollaa. Tämä varmistaa, että käyttäjän tunnisteet eivät lähetetä selkeätekstissä, mikä tekisi niistä helposti haavoittuvaisia tietoturvaloukkauksille.

See Also:
Voit lukea lisää HTTP-pyynnöistä ja perusautentikoinnista osoitteessa https://developer.mozilla.org/fi/docs/Web/HTTP/Authentication. Sieltä löydät myös tietoa muista autentikointimenetelmistä ja niiden käytöstä.