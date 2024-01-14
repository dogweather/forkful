---
title:                "Python: Lähettäessä http-pyynnön"
simple_title:         "Lähettäessä http-pyynnön"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen on tärkeä osa ohjelmoinnin maailmaa ja mahdollistaa kommunikoinnin eri palvelimien välillä. Tämä on erityisen hyödyllistä web-kehityksessä, sillä se mahdollistaa datan lähettämisen ja vastaanottamisen web-sovellusten välillä. 

## Näin se tapahtuu

HTTP-pyyntöjen lähettäminen Pythonilla on helppoa ja vaivatonta. Käytämme tätä esimerkkiä selventämään asiaa:

```Python
# Tuodaan request-kirjasto käyttöön
import requests

# Määritetään URL-osoite
url = "https://www.example.com"

# Lähetetään GET-pyyntö ja tallennetaan vastaus muuttujaan
response = requests.get(url)

# Tulostetaan vastauksen sisältö
print(response.text)
```

Tämä yksinkertainen esimerkki osoittaa, kuinka helposti voimme lähettää HTTP-pyyntöjä Pythonilla. Käytämme requests-kirjastoa, joka tekee pyyntöjen lähettämisestä vieläkin helpompaa.

Voimme myös lisätä parametreja ja muita tietoja pyyntöön tarpeemme mukaan. Esimerkiksi, jos haluamme lähettää POST-pyynnön ja lisätä siihen tietoa, se näyttäisi tältä:

```Python
# Tuodaan request-kirjasto käyttöön
import requests

# Määritetään URL-osoite
url = "https://www.example.com"

# Määritetään POST-pyynnön tiedot
data = {"username": "kayttajanimi", "password": "salasana"}

# Lähetetään POST-pyyntö
response = requests.post(url, data=data)

# Tulostetaan vastauksen sisältö
print(response.text)
```

## Syvemmälle asioihin

HTTP-pyyntöjen lähettämisessä on paljon muitakin vaihtoehtoja ja ominaisuuksia, jotka voivat olla hyödyllisiä erilaisissa tilanteissa. Voit esimerkiksi lisätä pyyntöön otsikoita tai asettaa aikarajoituksia.

Voit myös tarkastella vastauksen tilakoodia ja sen sisältämää dataa. Tämä auttaa sinua varmistamaan, että pyyntösi on onnistunut ja vastaanottanut haluamasi tiedot.

Pythonin requests-kirjaston dokumentaatiossa on paljon lisätietoa ja esimerkkejä, joita voit tutkia syvällisemmin, jos haluat oppia lisää HTTP-pyyntöjen lähettämisestä.

## Katso myös

- [Python requests-kirjaston dokumentaatio](https://requests.readthedocs.io/en/latest/)
- [HTTP-viestien merkitykset ja tilakoodit](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)