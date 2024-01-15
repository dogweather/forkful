---
title:                "Lähettämällä http-pyyntö"
html_title:           "Python: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi
Miksi kuka tahansa haluaisi lähettää HTTP-pyynnön? Yksinkertaisesti sanottuna, HTTP-pyynnöt mahdollistavat tiedon lähettämisen ja vastaanottamisen internetissä. Tämä on tärkeä osa verkkoon perustuvia sovelluksia ja palveluita.

## Kuinka Tehdä
Lähettäminen HTTP-pyynnön Pythonilla on helppoa. Käytä vain `requests`-kirjastoa ja seuraa näitä yksinkertaisia vaiheita.

```Python
import requests

# Luo olio, joka sisältää tarvittavat tiedot pyyntöä varten
pyynto = requests.get("https://www.example.com/")

# Lähetä pyyntö ja tallenna vastaus olio muuttujaan
vastaus = pyynto.json()

# Tulosta vastauksen sisältö
print(vastaus)
```

Tässä esimerkissä käytetään `requests`-kirjaston `get()`-metodia lähettämään GET-pyyntö ja tallentamaan vastaus `vastaus`-olioon. Käytämme myös `.json()`-metodia avataksesi vastauksessa olevan JSON-muotoisen tiedon. Lopuksi tulostamme vastauksen sisällön konsolille.

```
{'name': 'John', 'age': 30}
```

## Syvällinen Pohdinta
HTTP-pyyntöjen lähettäminen on yksinkertaista, mutta niiden avulla on mahdollista tehdä monimutkaisia asioita. Voit esimerkiksi lähettää POST-pyynnön ja lähettää tiedon muodossa, joka vastaa JavaScript-objektia, tai lisätä pyyntöön otsikoita ja evästeitä. Voit myös käyttää muita `requests`-kirjaston metodeja, kuten `put()`, `patch()` ja `delete()`.

## Katso Myös
- [Requests kirjaston dokumentaatio](https://requests.readthedocs.io/en/master/)
- [HTTP-oppiminen Pythonin avulla](https://realpython.com/python-requests/)