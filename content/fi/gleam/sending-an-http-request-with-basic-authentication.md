---
title:                "Gleam: Perusautentikoinnin lähettäminen HTTP-pyynnöllä"
simple_title:         "Perusautentikoinnin lähettäminen HTTP-pyynnöllä"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat lähettää HTTP-pyynnön perusautentikoinnilla? Vastaus on yksinkertainen - monet verkkosivustot ja palvelut vaativat käyttäjän tunnistautumisen ennen kuin ne antavat pääsyn tietyn sisällön tai toiminnon käyttämiseen. Perusautentikointi on yksi tapa varmistaa, että vain oikeilla käyttäjillä on pääsy näihin rajoitettuihin alueisiin.

## Kuinka

Gleam-kielen avulla voit helposti lähettää HTTP-pyynnön perusautentikoinnilla. Alla on esimerkki koodista, jossa lähetetään pyyntö API-osoitteeseen käyttäen perusautentikointia.

```Gleam
import http
import base64
import data.encoder

// Luodaan auth-otsake, joka sisältää käyttäjänimen ja salasanan koodattuna base64-muodossa
let auth_header = "Basic " ++ base64.encode("#{username}:#{password}")

// Luodaan pyyntölähetin, joka sisältää perusautentikoinnin otsakkeen
let request_sender = http.client({"Authorization": auth_header})

// Lähetetään GET-pyyntö API-osoitteeseen
let response = request_sender.get("https://example.com/api")

// Tulostetaan vastauksen sisältö
debug!("#{response.body}")
```

Yllä olevassa koodiesimerkissä importoidaan "http" -moduuli, joka tarjoaa työkaluja HTTP-pyynnön lähettämiseen ja vastauksen käsittelyyn. Base64-kirjaston avulla koodataan käyttäjänimi ja salasana ylläpitäjän vaatimaan muotoon. Luodaan sitten pyyntölähetin ja lisätään otsake, jossa on koodattu käyttäjänimi ja salasana. Lopuksi lähetetään GET-pyyntö API-osoitteeseen ja tulostetaan vastauksen sisältö.

Mahdollinen tulostus voisi olla:

```
"{\"message\": \"Tervetuloa API:in!\"}"
```

## Syväsukellus

HTTP-pyynnön lähettäminen perusautentikoinnilla voi tuntua monimutkaiselta, mutta Gleam-kielen avulla se on yksinkertaista. Käyttämällä "http" -moduulin tarjoamia työkaluja ja base64-kirjastoa, voit luoda helposti tarvittavan HTTP-pyynnön ja lähettää sen haluamaasi verkkopalveluun tai API:hin. Täyden dokumentaation Gleam-kielen http-moduulista löydät täältä: [https://gleam.run/modules/http.html](https://gleam.run/modules/http.html).

## Katso myös

- [Gleam-kielen virallinen verkkosivusto](https://gleam.run/)
- [Gleam-kielen dokumentaatio](https://gleam.run/docs/)