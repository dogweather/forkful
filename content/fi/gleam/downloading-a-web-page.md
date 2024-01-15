---
title:                "Web-sivun lataaminen"
html_title:           "Gleam: Web-sivun lataaminen"
simple_title:         "Web-sivun lataaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit ladata verkkosivun? Ehkä työskentelet webskraping-projektissa tai haluat vain tallentaa sivun myöhempää käyttöä varten. Riippumatta syystä, Gleam tarjoaa helpon ja tehokkaan tavan ladata verkkosivuja.

## Kuinka

Annamme sinulle esimerkin kuinka ladata verkkosivu käyttäen Gleamia:

```Gleam import Html download
  
  url = "https://www.example.com"
  response = Html.download(url)
  page = response.body
  print(page)
```

Tämä palauttaa sivun HTML-koodin ja tulostaa sen konsoliin.

## Syvällinen tarkastelu

Gleamilla on erityisiä toimintoja, kuten `download_with_timeout` ja `download_with_headers`, jotka antavat sinun säätää latausprosessia. Voit myös käyttää `download_to_file` tallentaaksesi sivun suoraan tiedostoon.

## Katso myös

- [Gleamin virallinen dokumentaatio](https://gleam.run/documentation/)
- [Codecademyn opetusohjelma Gleamista](https://www.codecademy.com/learn/learn-gleam)