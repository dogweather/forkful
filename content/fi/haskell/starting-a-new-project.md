---
title:                "Haskell: Aloittaminen uudella projektilla."
simple_title:         "Aloittaminen uudella projektilla."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Miksi aloittaa uusi Haskell-projekti? Haskell on vahvasti tyypitetty, puhtaasti funktionaalinen ohjelmointikieli, joka mahdollistaa tehokkaan ja turvallisen koodin kirjoittamisen. Tämä tekee siitä loistavan vaihtoehdon monimutkaisiin ja vaativiin projekteihin.

## Miten

Aloittaaksesi uuden projektin Haskellissa, sinun tulee ensin asentaa GHC (Glasgow Haskell Compiler). Voit tehdä tämän käyttämällä suosittua työkalua nimeltä Cabal, joka hallinnoi projektien riippuvuuksia ja rakentaa tarvittavat kirjastot. 

```Haskell
cabal install ghc
```

Tämän jälkeen voit luoda uuden projektin komennolla:

```Haskell
cabal init
```

Tämä luo projektisi juurihakemistoon tiedostonimeltä `my-project.cabal`, joka sisältää projektisi metatiedot, kuten riippuvuudet ja tärkeimmät tiedostot. Voit nyt muokata tätä tiedostoa vastaamaan projektisi tarpeita ja aloittaa koodaamisen! 

## Syvällinen sukellus

Muutamia huomioon otettavia asioita aloittaessasi uutta Haskell-projektia:

- Kirjoita selkeää ja luettavaa koodia. Haskellissa on vahva tyypitys ja tyyppitiedot auttavat ymmärtämään koodin tarkoitusta ja rakennetta. 
- Hyödynnä funktionaalisen ohjelmoinnin paradigmoja, kuten rekursiota ja korkeamman asteen funktioita. Nämä auttavat kirjoittamaan lyhyempää ja selvempää koodia. 
- Käytä muuttumattomia arvoja aina kun mahdollista. Muuttumattomuus tekee koodista turvallisempaa ja helpottaa koodin ymmärtämistä. 
- Käytä kommentteja ja dokumentaatiota selittämään koodisi toimintaa ja tarkoitusta. Näin muut voivat helposti ymmärtää koodisi ja sinä itse pystyt palauttamaan mieleesi vanhoja toimintoja. 

## Katso myös

- [Haskellin aloittelijan opas](https://www.haskell.org/documentation/)
- [Cabal-hallintajärjestelmä](https://wiki.haskell.org/Cabal-Introduction)