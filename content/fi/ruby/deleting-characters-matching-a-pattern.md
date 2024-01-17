---
title:                "Mallin mukaisten merkkien poistaminen"
html_title:           "Ruby: Mallin mukaisten merkkien poistaminen"
simple_title:         "Mallin mukaisten merkkien poistaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Poistaessasi merkkejä, jotka vastaavat tiettyä kuviona olevaa merkkijonoa, tarkoittaa sitä, että poistat kaikki merkit, jotka täyttävät määritetyn kuvion. Tämä voi olla hyödyllistä, kun haluat puhdistaa tekstin tai datan turhista merkeistä. Ohjelmoinnissa tätä tehdään usein automatisoinnin ja tietojenkäsittelyn helpottamiseksi.

## Miten:
```Ruby
# Esimerkki koodista
text = "Tämä on esimerkki tekstistä!"
text = text.gsub(/[ä!,]/,"") # poistaa ä ja ! merkit

puts text # tulostaa "Tm on esimerkki tekstist" (huomaa poistetut merkit)

```
```Ruby
# Toinen esimerkki: poistaa kaikki välilyönnit tekstistä
text = "Tämä on toinen esimerkki!"
text = text.delete(" ") # poistaa kaikki välilyönnit

puts text # tulostaa "Tämäontoinenesimerkki!"
```

## Syvällinen sukellus:
Poistaminen merkkejä vastaavien kuvion avulla on ollut käytössä ohjelmoinnissa jo pitkään ja sitä käytetään yhä laajasti automatisoinnin ja datan käsittelyn yhteydessä. Vaihtoehtoisesti voit myös käyttää ohjelmointikielinä muita vaihtoehtoisia menetelmiä, kuten split() tai scan() Rubyssä. Tämä toiminto vaatii myös tarkkaa määrittelyä halutuista merkeistä, jotta vahingossa ei poisteta tärkeitä merkkejä.

## Katso myös:
Voit tutustua lisää Ruby ohjelmointikielen mahdollisuuksiin virallisilta nettisivuilta: <https://www.ruby-lang.org/fi/>