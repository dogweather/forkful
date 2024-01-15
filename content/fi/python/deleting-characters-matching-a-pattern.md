---
title:                "Kuvioiden mukaisesti vastaavien merkkien poistaminen."
html_title:           "Python: Kuvioiden mukaisesti vastaavien merkkien poistaminen."
simple_title:         "Kuvioiden mukaisesti vastaavien merkkien poistaminen."
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##Miksi

Olet varmaankin törmännyt tilanteeseen, jossa joudut poistamaan tietyn mallin mukaisia merkkejä tekstitiedostosta. Tämä voi johtua esimerkiksi tekstin puhdistamisesta ennen analyysiä tai halusta muokata tekstiä tietyn muodon mukaiseksi. Tässä artikkelissa näytetään, kuinka voit poistaa merkkejä, jotka vastaavat tiettyä mallia käyttämällä Pythonin uusinta versiota.

##Kuinka tehdä

Poistaaksesi merkkejä mallin perusteella, voit käyttää Pythonin built-in replace() -funktiota. Esimerkiksi, jos haluat poistaa kaikki numerot tekstitiedostosta, voit kirjoittaa seuraavan koodin:

```Python
teksti = '13m45e12r45k25k15i18t22'
puhdistettu_teksti = teksti.replace('[0-9]','')
print(puhdistettu_teksti)
```

Tässä koodissa vaihdamme kaikki numerot tyhjään merkkijonoon, jolloin numerot poistetaan tekstistä. Tämän tulisi antaa seuraava tulos:

```Python
merkkiiit
```

Voit myös käyttää regular expression -kirjastoa (regex) poistaaksesi merkkejä tietyn mallin perusteella. Tässä esimerkissä käytämme regex:n sub() -funktiota, joka korvaa kaikki numerot tyhjällä merkkijonolla:

```Python
import re
teksti = '13m45e12r45k25k15i18t22'
puhdistettu_teksti = re.sub('[0-9]','',teksti)
print(puhdistettu_teksti)
```

Tulisi saada sama tulos kuin edellisessä esimerkissä.

##Syventyminen

Nämä esimerkit kattavat vain yksinkertaisimmat tapaukset poistaa merkkejä mallin perusteella. Voit kuitenkin käyttää regex:n laajempia toimintoja, kuten merkkien ryhmittelyä tai kerrannaisuuksien korvaamista, poistaaksesi monimutkaisempia malleja vastaavia merkkejä. Tutustumalla tarkemmin regex-kirjaston dokumentaatioon voit löytää lisää tapoja poistaa merkkejä haluamallasi tavalla.

##Katso myös

- [Pythonin string -dokumentaatio](https://docs.python.org/3/library/string.html)
- [Regex-kirjaston dokumentaatio](https://docs.python.org/3/library/re.html)