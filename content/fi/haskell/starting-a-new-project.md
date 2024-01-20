---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Aloitetaan uusi projekti? Saatat miettiä mikä se on? Tietokoneohjelmoinnissa tarkoitamme uutta sovellusta tai ohjelmakirjastoa, jonka aiomme kehittää. Ja miksi me teemme sen? Luomme uusia projekteja, jotta voimme ratkaista ongelmia, tarjota uusia palveluita tai parantaa olemassa olevia järjestelmiä.

## Kuinka tehdä:
Haskellilla projekti aloitetaan yleensä käyttämällä "stack" työkalua. Katsotaanpa esimerkkiä:
```Haskell
-- Asennetaan ensin stack:
$ curl -sSL https://get.haskellstack.org/ | sh

-- Aloitetaan uusi projekti nimeltä "awesomeProject":
$ stack new awesomeProject simple
```
Nämä komennot asentavat stackin ja luovat uuden projektin. Projekti koostuu useista tiedostoista ja hakemistoista. Tärkein on "awesomeProject.cabal", joka määrittää projektin rakenteen ja riippuvuudet.

## Sudenkuoppia:
Aloitetaanko uusi projekti nollasta, ei ole aina itsestään selvää. Edeltäjäkoodien käyttö voi säästää aikaa, mutta sen on oltava huolellisesti suunniteltu ja testattu. Historiallisesta näkökulmasta Haskellin kehitystyökalut, kuten "stack", ovat kuuluneet kielen standardipakettiin vuodesta 2015. 

On muitakin tapoja aloittaa projekti Haskellilla, esimerkiksi "Cabal". Mutta useimmat Haskellers suosivat "stackia", koska se pitää projektin riippuvuudet eristettyinä ja hallittavissa. 

Uuden Haskell-projektin toteutuksessa pitää varoa monimutkaisten tyyppejä - modularisoi koodisi oikein ja pidä riippuvuudet minimalilla.

## Katso myös:
Jos haluat oppia lisää Haskellista ja projektien aloittamisesta, tässä on joitakin linkkejä:

1.  Haskellin viralliset sivut: http://www.haskell.org/
2.  Stackin dokumentaatio: https://docs.haskellstack.org/
3.  "Real World Haskell" kirja: http://book.realworldhaskell.org/
4.  Haskellin opiskeluoppaat: https://learn.haskell.org/

Hyvää koodaamista!