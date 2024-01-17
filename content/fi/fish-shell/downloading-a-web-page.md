---
title:                "Verkkosivun lataaminen"
html_title:           "Fish Shell: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Mitä & Miksi?

Lataaminen web-sivun ohjelmointina tarkoittaa että tiedot sivulta tallennetaan tietokoneelle. Tämä voi olla hyödyllistä esimerkiksi tietojen keräämiseen tai automatisointiin.

Miten:

Fish Shellin avulla voit ladata web-sivun helposti muutamalla rivillä koodia. Käytämme curl-komentoa lähteenä ja tallennamme sivun tiedot muuttujaan "sivu":

```Fish Shell
sivu = curl -s <URL>
```

Voimme sitten tulostaa sivun sisällön käyttämällä "echo" komentoa:

```Fish Shell
echo $sivu
```

Tämä tulostaa kaiken sivun sisällön konsolille. Voit myös tallentaa tiedot tiedostoon käyttämällä "tee" komentoa:

```Fish Shell
tee sivu.html <<< $sivu
```

## Syvällistä tietoa:

Lataaminen web-sivun ei ole uusi käsite, mutta se on yleistynyt entisestään nopeiden internetyhteyksien myötä. On myös muita tapoja ladata sivuja, kuten esimerkiksi käyttäen Pythonin "urllib" kirjastoa.

"curl" komento on yleisesti käytössä Unix-järjestelmissä ja tarjoaa useita erilaisia ominaisuuksia, kuten esimerkiksi latausten jatkaminen ja verkkotunnusten tunnistaminen.

## Katso myös:

* [Fish Shell dokumentaatio](https://fishshell.com/docs/current/index.html)
* [Curl manuaali](https://curl.se/docs/manpage.html)
* [Urllib dokumentaatio](https://docs.python.org/3/library/urllib.html)