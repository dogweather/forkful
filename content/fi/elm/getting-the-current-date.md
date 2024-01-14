---
title:    "Elm: Nykyisen päivämäärän saaminen"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Miksi käyttää nykyisen päivämäärän hankintaa?

Jokainen ohjelmoija tarvitsee ajoittain kykyä hakea nykyinen päivämäärä. Se voi olla hyödyllistä esimerkiksi luotaessa sovelluksia tapahtumakalentereille tai aikaperusteisten toimintojen suorittamiselle. Elm-ohjelmointikieli tarjoaa helpon ja tehokkaan tavan hakea nykyinen päivämäärä.

## Kuinka tehdä se?

Onneksi Elmillä on sisäänrakennettu funktio, joka mahdollistaa nykyisen päivämäärän hankkimisen. Se on nimeltään `Time.Now` ja se palauttaa `Posix`-tyypin arvon, joka edustaa tämänhetkistä aikaa.

```Elm
import Time

-- haetaan nykyinen päivämäärä
currentDate = Time.now
```

Tämä funktio palauttaa `Posix`-tyypin käärittynä `Result`-tyyppiin. Tämä tarkoittaa sitä, että on mahdollista, että jotain menee vikaan, jolloin `Result`-tyyppi sisältää virheen sijaan `Posix`-tyypin arvon. Tämä pitäisi ottaa huomioon, kun käsittelet nykyisen päivämäärän hankintaa.

Jos haluat tarkastella nykyisen päivämäärän lisäksi myös kellonaikaa, voit käyttää `Time.millisToPosix`-funktiota muuntaaksesi millisekunnit `Posix`-tyypiksi.

## Syvällinen sukellus

Kun käytät `Time.now` -funktiota, Elm palauttaa nykyisen päivämäärän UTC-aikavyöhykkeessä. Jos haluat muuttaa aikavyöhykettä, voit käyttää `Time.inTimeZone` -funktiota ja antaa sille haluamasi aikavyöhykkeen `String`-muodossa.

```Elm
import Time

-- haetaan nykyinen päivämäärä ja kellonaika Euroopan aikavyöhykkeelle muunnettuna
currentDateTime = Time.inTimeZone "Europe/Helsinki" Time.now
```

On myös tärkeää huomata, että `Time.now` palauttaa ajan millisekunteina. Jos haluat näyttää nykyisen päivämäärän ja kellonajan tietyn muodon mukaan, voit käyttää `Time.format`-funktiota, joka hyödyntää `iso8601`-formaatista.

```Elm
import Time

-- haetaan nykyinen päivämäärä ja formaatataan se stringiksi
currentDateTime = Time.format Iso8601 Time.now
```

Tässä esimerkissä käytetään `Iso8601`-formaatia, mutta on myös muita vaihtoehtoja, kuten `Rfc3339`, `Rfc1123` ja `Rfc822`.

# Katso myös

- [Elm dokumentaatio](https://elm-lang.org/docs)
- [Elm Weekly](https://elmweekly.nl)
- [Finnish Elm Slack-yhteisö](https://elmslack.com)