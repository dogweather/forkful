---
title:                "Verkkosivun lataaminen"
html_title:           "Python: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Monet meistä haluavat tallentaa suosikki verkkosivumme paikallisesti tai käyttää niitä myöhemmin offline-tilassa. Pythonin avulla voimme helposti ladata verkkosivuja ja tallentaa ne tiedostomuotoon, jota voimme käyttää myöhemmin.

## Miten

Kaiken alku on pyytää tarvittavaa Python-kirjastoa, josta löydät verkkosivujen lataamiseen tarvittavat työkalut. Voit tehdä tämän käyttämällä "pip" -komentoa ja asentamalla "requests" -kirjaston:

```Python
pip install requests
```

Kun olet ladannut "requests" -kirjaston, voit käyttää sitä Pythonissa yksinkertaisella koodilla:

```Python
import requests

page = requests.get('https://www.example.com')

print(page.status_code)
```

Koodi käyttää pyyntökirjastoa lähettämään pyynnön verkkosivulle ja sitten tallentamaan vastauksen "page" -muuttujaan. Tämän jälkeen voimme käyttää "status_code" -ominaisuutta selvittääksemme, onko sivu latautunut onnistuneesti. Voit myös käyttää muita "requests"-kirjaston ominaisuuksia, kuten "text" tai "content", selvittääksesi sivun sisällön.

## Syvä sukellus

Kun haluat ladata monimutkaisempia verkkosivuja, saatat joutua lisäämään lisäparametreja pyyntöön. "Requests" -kirjasto tarjoaa monia vaihtoehtoja, kuten käyttäjäagentin määrittämistä, evästeiden lähettämistä ja jopa "HTTPS"-tukea. Voit tutustua kaikkiin näihin vaihtoehtoihin "requests"-kirjaston dokumentaatiosta.

Voit myös käyttää muita Python-kirjastoja, kuten "BeautifulSoup" tai "Scrapy" lisätäksesi sivujen lataamiseen liittyviä ominaisuuksia, kuten tiedon perkaamista tai sivustojen välistä siirtymistä.

## Katso myös

- [Requests kirjaston dokumentaatio] (https://requests.readthedocs.io/en/master/)
- [Python ohjelmoinnin aloittamisopas] (https://www.python.org/about/gettingstarted/)
- [BeautifulSoup ohjelmointikielen ohjelmointiopas] (https://www.crummy.com/software/BeautifulSoup/bs4/doc/)