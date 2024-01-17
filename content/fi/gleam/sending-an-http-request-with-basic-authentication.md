---
title:                "Lähettämässä http-pyyntöä perusautentikoinnilla"
html_title:           "Gleam: Lähettämässä http-pyyntöä perusautentikoinnilla"
simple_title:         "Lähettämässä http-pyyntöä perusautentikoinnilla"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Mikä & Miksi?
Lähettämällä HTTP-pyynnön perustason tunnistautumisella tarkoitetaan sitä, että käyttäjä lähettää pyynnön verkkopalvelimelle, joka vaatii käyttäjän tunnistautumisen. Tämä tapahtuu lisäämällä käyttäjän käyttäjätunnus ja salasana pyyntöön. Ohjelmoijat tekevät tätä voidakseen tehdä asioita, kuten lähettää tietoja palvelimelle tai hakea tietoja tietokannasta.

Miten:
Esimerkiksi voidaksesi hakea tietoja käyttäjätunnuksellasi ja salasanallasi suojatulta API:lta, sinun on lisättävä pyyntöösi käyttäjätunnus ja salasana. Tämä tapahtuu käyttämällä Gleamin ```Gleam http.send``` ja ```Gleam http.basic_auth``` funktioita. Alla on esimerkit siitä, miten tämä tehdään:

```Gleam
http.send(
    url: "https://api.example.com/data",
    method: Http.Get,
    headers: [basic_auth("kayttajatunnus", "salasana")],
  )
```

Tuloksen pitäisi olla vastaus, joka sisältää halutut tiedot.

Syvimmälle sukellus:
Peruskäyttäjätunnistus on yksi monista käytetyistä tunnistusmenetelmistä HTTP-pyynnöissä. Siinä käyttäjän tunnistautumistiedot (käyttäjätunnus ja salasana) toimitetaan Base64-muodossa ja lisätään pyyntöön otsikkoon ```Authorization```. Tämä menetelmä on ollut käytössä jo vuodesta 1999 ja se on yksi yksinkertaisimmista tavoista varmistaa, että käyttäjän tiedot ovat suojassa.

Vaihtoehtoja ovat esimerkiksi Digest-tunnistautuminen, jossa käyttäjän salasanaa ei tallenneta selkokielisenä tietokantaan, sekä OAuth, jossa käyttäjä tunnistetaan palveluntarjoajan sivuston kautta.

Katso myös:
- [Gleamin HTTP-moduuli](https://gleam.run/modules/http)
- [Base64-koodekointi](https://fi.wikipedia.org/wiki/Base64)