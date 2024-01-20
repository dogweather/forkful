---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Webbisivun lataaminen tarkoittaa sen sisällön kopioimista omalle koneellesi. Ohjelmoijat tekevät tämän syystä tai toisesta - ehkä he haluavat käsitellä sivun tietoja, tai ehkä he tarvitsevat offline-versiota.

## Kuinka:
Bash-skriptin avulla voit ladata webbisivuja nopeasti ja vaivattomasti. Pieni esimerkkikoodi voisi näyttää tältä:

```bash
#!/bin/bash
# Sivun URL
url="http://example.com"

# Lataa sivu
curl -o my_page.html $url
```

Kun ajat yllä olevan skriptin, luo se tiedoston `my_page.html`, joka sisältää koko sivun HTML-koodin. 

## Syvällisemmin:
Webbisivun lataaminen on vanha konsepti, se oli netin alkuaikoina ohjelmoijien ainoa tapa käsitellä verkkosisältöä. Vaihtoehtoja on nykyään paljon, esim. API-kutsut tai päätepistekyselyt.

Tämän Bash-skriptin toteutus käyttää `curl`-komentoa, joka on voimakas komentorivityökalu tiedon siirtämiseen verkossa. Se on olemassa lähes kaikissa UNIX-pohjaisissa käyttöjärjestelmissä.

## Katso myös:
- [Curl-komennon dokumentaatio](https://curl.se/docs/manpage.html)
- [Verkkosisällön käsittely Bashilla](https://www.linuxjournal.com/content/downloading-entire-web-site-wget)
- [API-kyselyt Bashissa](https://linuxize.com/post/curl-rest-api/)