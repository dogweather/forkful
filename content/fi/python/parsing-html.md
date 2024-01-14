---
title:                "Python: Html-analysointi"
simple_title:         "Html-analysointi"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Usein verkkosivustoista halutaan saada tietoa automatisoidusti esimerkiksi datan keräämiseksi tai sivujen analysoimiseksi. Tällöin HTML-tiedostojen lukeminen ja tietojen parsiminen on välttämätöntä.

## Kuinka

Parsiminen on prosessi, jossa HTML-tiedostosta luetaan tarvittavat tiedot ja niitä käsitellään halutulla tavalla. Tämä voidaan tehdä monella eri tavalla, mutta useimmiten käytetään Python-ohjelmointikieltä sen monipuolisten kirjastojen ansiosta.

```Python
# Tuodaan BeautifulSoup-kirjasto
from bs4 import BeautifulSoup 

# Luodaan esimerkki HTML-tiedosto
html = "<html><body><h1> Tervetuloa </h1><p> Tämä on esimerkkisivu </p></body></html>"

# Parsitaan HTML ja tallennetaan sivun otsikko
soup = BeautifulSoup(html, "html.parser")
otsikko = soup.find("h1").get_text()

# Tulostetaan otsikko
print(otsikko)
```

```
Output: Tervetuloa
```

## Syväsyventyminen

HTML:n parsimisessa on tärkeää ymmärtää tiedoston rakennetta ja miten halutut tiedot voidaan löytää. BeautifulSoup-kirjasto tarjoaa käteviä työkaluja, kuten find() ja find_all(), joiden avulla tiettyjä elementtejä voidaan hakea sivulta. Lisäksi CSS- ja XPath-määrittelyt voidaan käyttää tarkentamaan hakuja. Tämä tekee HTML:n parsimisesta joustavaa ja tehokasta.

## Katso myös

- Seuraavassa [linkissä](https://www.crummy.com/software/BeautifulSoup/bs4/doc/) on tarkempia ohjeita BeautifulSoup-kirjaston käyttöön.
- Tutustu myös [requests-kirjastoon](https://requests.readthedocs.io/en/master/), jonka avulla voidaan hakea HTML-tiedostoja suoraan verkosta.
- Opit lisää Python-ohjelmoinnista [Codecademy-sivustolla](https://www.codecademy.com/learn/learn-python?utm_source=google&utm_medium=organic&utm_campaign=learnpython&utm_content=pythonhomepage).