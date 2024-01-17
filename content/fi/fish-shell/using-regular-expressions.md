---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Fish Shell: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Regular expressionien käyttö tarkoittaa lyhyesti sanottuna tietynlaisten merkkijonojen etsimistä ja manipulointia ohjelmoinnissa. Tämä on erityisen hyödyllistä silloin, kun työskennellään suurten ja monimutkaisten tietomassojen kanssa, sillä regex:ien avulla voidaan nopeasti ja tarkasti löytää haluttuja kappaleita tekstistä. Näin säästytään turhalta manuaaliselta etsimiseltä ja korjailulta.

## Miten:
Koodin avulla on helpointa havainnollistaa regex:ien käyttöä. Alla on esimerkkejä Fish Shell -komentorivityyppisillä koodiblokeilla.

```
Fish Shell Regular Expression

# Hakee ja tulostaa kaikki sähköpostiosoitteet tekstitiedostosta
pcregrep -o '\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,6}\b' tiedosto.txt

# Korvaa kaikki välilyönnit tekstistä väliviivalla
sed 's/ /-/g' tiedosto.txt
```

## Syvempi sukellus:
On hyödyllistä tietää, että regular expressionit ovat olleet käytössä jo pitkään ja ne ovat lähes välttämättömiä ohjelmoinnin maailmassa. On myös olemassa muita vaihtoehtoisia tapoja käsitellä merkkijonoja, kuten esimerkiksi string-metodeilla, mutta regex:it tarjoavat monipuoliset ja tehokkaat työkalut. Fish Shell käyttää PCRE (Perl Compatible Regular Expression) -kirjastoa, joka mahdollistaa monimutkaisempien regex:ien käytön.

## Katso myös:
- [Fish Shell -dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Regex-tutoriaali ja -harjoituksia](https://regexone.com/)