---
title:                "Arduino: Lähetetään http-pyyntö perusautentikoinnin kanssa."
simple_title:         "Lähetetään http-pyyntö perusautentikoinnin kanssa."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit lähettää HTTP-pyynnön perusautentikoinnilla Arduinon avulla? Yksinkertaisesti sanottuna, HTTP-pyyntöjen lähettäminen avauttaa mahdollisuuksia kommunikoida muiden verkkopalveluiden kanssa ja saada tietoa niistä. Perusautentikointi on yksi tapa varmistaa, että vain oikeutetut käyttäjät voivat päästä näihin palveluihin.

## Miten

Aloitetaan lähettämällä HTTP-pyyntö perusautentikoinnilla esimerkkikoodin avulla:

```Arduino
// Lähetetään HTTP-pyyntö POST-metodilla
HTTPClient http;
http.begin("https://api.example.com/endpoint");
http.setAuthorization("käyttäjänimi", "salasana");
int httpCode = http.POST("Pyyntösisältö");
String vastaus = http.getString();
Serial.println(httpCode); // Tulostaa vastauksen koodin
Serial.println(vastaus); // Tulostaa vastauksen sisällön
http.end();
```

Tässä koodissa käytämme HTTPClient-kirjastoa, joka helpottaa HTTP-pyyntöjen lähettämistä. Aluksi määrittelemme pyynnön osoitteen `begin()`-funktiolla ja lisäämme siihen `setAuthorization()`-funktiolla käyttäjänimen ja salasanan. Tämän jälkeen käytämme `POST()`-funktiota lähettämään pyyntösisällön ja tallennamme palautetun koodin sekä sisällön `httpCode` ja `vastaus` muuttujiin. Lopuksi meidän täytyy kutsua `end()`-funktiota vapauttaaksemme HTTP-yhteyden.

Huomaa, että voit myös käyttää muita metodeja kuten `GET`, `PUT` ja `DELETE` riippuen siitä, mikä on pyynnön tarkoitus ja verkkopalvelun tarjoamat mahdollisuudet.

## Syvemmälle

HTTP-pyyntöjen lähettäminen perustuu protokollaan, jota kutsumme HTTP:ksi. Tämän protokollan yksityiskohdat eivät kuulu tämän artikkelin puitteisiin, mutta on hyödyllistä ymmärtää muutama asia.

Ensisijainen HTTP-pyyntö tapahtuu kahdella osalla: otsakkeella (header) ja sisällöllä (body). Otsake sisältää metatietoa pyynnöstä, kuten käytetty protokolla, käyttäjänimi ja salasana, ja sisältö taas sisältää varsinaisen pyyntösisällön. Perusautentikoinnin tapauksessa käyttäjänimi ja salasana lähetetään otsakkeessa Base64-koodattuna. Tämä on vain yksi tapa suojata yhteys ja useimmat verkkopalvelut tarjoavat muita vaihtoehtoja.

Vastaavasti HTTP-pyynnön vastaus koostuu myös otsakkeesta ja sisällöstä. Otsake sisältää vastauksen koodin, joka kertoo onnistuiko pyyntö vai ei, ja sisältö taas sisältää varsinaisen vastauksen sisällön. Tähän voidaan sisällyttää esimerkiksi haluttu data tai virheilmoitukset.

On myös tärkeä huomata, että proxy-palvelimen kanssa kommunikoidessa täytyy käyttää erityisiä otsaketietoja ja itse proxy saattaa vaatia oman autentikointinsa. Tämä vaatii lisämuutoksia pyyntöön ja täytyy selvittää kunkin proxy-palvelimen kohdalla erikseen.

## Katso myös

- [Arduino virallinen sivu](