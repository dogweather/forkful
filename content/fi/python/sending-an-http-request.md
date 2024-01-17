---
title:                "Lähettämällä http-kysely"
html_title:           "Python: Lähettämällä http-kysely"
simple_title:         "Lähettämällä http-kysely"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Miten & Miksi?
Lähes jokainen verkkosivusto ja sovellus tarvitsee tietoa palvelimiltaan jotta se voi toimia oikein. Tämä tieto haetaan lähettämällä HTTP-pyyntöjä, jotka ovat viestejä palvelimille. Ohjelmoijat käyttävät HTTP-pyyntöjä kommunikoidakseen palvelimien kanssa ja saadakseen haluamansa tiedot.

## Miten:
Pythonilla on helppo lähettää HTTP-pyyntöjä käyttäen valmiita kirjastoja, kuten requests. Voit lähettää GET-pyynnön esimerkiksi seuraavalla koodilla:
```Python
import requests
response = requests.get('https://example.com')
print(response.status_code)
```
Tämä koodi lähettää GET-pyynnön sivustolle "https://example.com" ja tulostaa vastauksen statuskoodin, joka kertoo pyynnön onnistumisesta.

## Syvemmälle:
HTTP-pyyntöjen historia juontaa juurensa takaisin vuoteen 1991, kun Tim Berners-Lee kehitti HTTP-protokollan. Tänä päivänä on olemassa muitakin vaihtoehtoja HTTP:lle, kuten HTTPS, joka käyttää salausta tietoturvan parantamiseksi. Lisäksi Pythonilla on muitakin kirjastoja, kuten urllib, joilla voi lähettää HTTP-pyyntöjä.

## Katso myös:
Voit lukea lisää HTTP-pyyntöjen lähettämisestä Pythonilla täältä: https://docs.python-requests.org/en/master/user/quickstart/
Löydät myös tietoa urllib-kirjastosta täältä: https://docs.python.org/3/library/urllib.html