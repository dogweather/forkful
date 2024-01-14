---
title:                "Fish Shell: Säännöllisten lausekkeiden käyttö"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Olet ehkä kuullut termistä "säännöllinen lauseke" eli "regular expression", mutta miksi niitä kannattaa käyttää? Säännölliset lausekkeet ovat erittäin hyödyllisiä, kun haluat etsiä, korvata tai manipuloida tekstimuotoista tietoa. Ne ovat erityisen käteviä, kun työskentelet Fish Shell -ohjelmointiympäristössä.

## Kuinka

Fish Shellilla on sisäänrakennettu tukea säännöllisille lausekkeille, ja niitä käytetään komentojen määrittämiseen. Voit käyttää niitä myös suoraan komentoriviltä vaihdettuasi tilaan "math". Tässä on muutamia esimerkkejä käytöstä Fish Shellissa.

```
Glucose testi | yhteenveto
```
Tämä komento etsii kaikki tiedostot, jotka sisältävät sanat "Glucose" ja "testi" ja antaa yhteenvedon niiden sisällöstä.

```
Etsi "koira" * .txt
```
Tämä komento etsii kaikki .txt-päätteiset tiedostot, joissa esiintyy sana "koira".

```
Säännöllisyys
```
Tämä komento etsii kaikki tiedostot ja tulostaa vain sääntöjä vastaavat rivit.

## Syväsukellus

Säännölliset lausekkeet perustuvat matemaattisiin malleihin, jotka kuvaavat merkkijonojen rakennetta. Niissä käytetään erilaisia symboleita ja operaatioita, jotka auttavat määrittämään mitä etsitään. Voit lukea lisää säännöllisistä lausekkeista Fish Shellin dokumentaatiosta tai muista lähteistä.

Säännöllisten lausekkeiden käyttöön opettelu voi aluksi tuntua vaikealta, mutta harjoittelemalla niitä säännöllisesti ne tulevat tutummiksi ja niihin oppii löytämään omia tapojaan käyttää niitä.

## Katso myös

- [Fish Shellin dokumentaatio](https://fishshell.com/docs/current/cmds/re_match.html)
- [RegExr-sivusto, jolla voi testata säännöllisiä lausekkeita](https://regexr.com)
- [Säännöllinen lauseke - Wikipedia](https://fi.wikipedia.org/wiki/S%C3%A4%C3%A4nn%C3%B6llinen_lauseke)