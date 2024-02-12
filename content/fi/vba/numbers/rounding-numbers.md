---
title:                "Lukujen pyöristäminen"
aliases:
- fi/vba/rounding-numbers.md
date:                  2024-02-01T22:02:17.890727-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lukujen pyöristäminen"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/vba/rounding-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Numeroiden pyöristäminen ohjelmoinnissa tarkoittaa numeron likimääräistä pyöristämistä lähimpään kokonaislukuun tai tiettyyn desimaalipaikkojen määrään. Ohjelmoijat pyöristävät numeroita yksinkertaistaakseen lukuja, parantaakseen luettavuutta tai täyttääkseen tietyt numeeriset kriteerit laskelmissa, erityisesti taloudellisissa laskelmissa, joissa tarkkuus on tärkeää.

## Kuinka tehdä:

Visual Basic for Applications (VBA) -ohjelmoinnissa pyöristäminen voidaan saavuttaa käyttämällä useita funktioita, joista jokainen sopii tiettyihin tilanteisiin. Tässä ovat yleisimmin käytetyt funktiot esimerkein:

1. **Round-funktio**:
   `Round`-funktio pyöristää numeron määriteltyyn numeroiden määrään.
   ```basic
   Dim roundedNumber As Double
   roundedNumber = Round(3.14159, 2)  ' Tuloste: 3.14
   MsgBox roundedNumber
   ```
   
2. **Int- ja Fix-funktiot**:
   Sekä `Int`- että `Fix`-funktiot käytetään numeroiden pyöristämiseen alaspäin lähimpään kokonaislukuun, mutta ne käyttäytyvät eri tavalla negatiivisten numeroiden kanssa.
   ```basic
   Dim intRounded As Integer
   Dim fixRounded As Integer
   
   intRounded = Int(-3.14159)  ' Tuloste: -4
   fixRounded = Fix(-3.14159)  ' Tuloste: -3
   
   MsgBox "Int: " & intRounded & ", Fix: " & fixRounded
   ```

3. **Ceiling- ja Floor-funktiot**:
   VBA:ssa ei ole oletuksena `Ceiling`- ja `Floor`-funktioita, jotka löytyvät muista kielistä. Tämän simulointiin voi käyttää `Application.WorksheetFunction.Ceiling_Math`- ja `Application.WorksheetFunction.Floor_Math` -funktioita Excel VBA:ssa.
   ```basic
   Dim ceilingNumber As Double
   Dim floorNumber As Double
   
   ceilingNumber = Application.WorksheetFunction.Ceiling_Math(3.14159)  ' Tuloste: 4
   floorNumber = Application.WorksheetFunction.Floor_Math(3.14159)  ' Tuloste: 3
   
   MsgBox "Ceiling: " & ceilingNumber & ", Floor: " & floorNumber
   ```

## Syväsukellus

VBA:n `Round`-funktio eroaa muiden kielten pyöristysmenetelmistä sen käyttämän **Banker's Rounding** -pyöristystavan vuoksi. Bankerin pyöristys pyöristää lähimpään parilliseen numeroon silloin, kun ollaan täsmälleen kahden numeron välissä, mikä vähentää harhautumista laskelmissa suuren tietoaineiston yli ja tarjoaa tilastollisesti merkittävämmän tuloksen. Tämä voi kuitenkin johtaa odottamattomaan käytökseen niille, jotka eivät ole tottuneet siihen, erityisesti kun jokaisessa tapauksessa odotetaan tarkan tarkkuuden saavuttamista.

Toisin kuin monet ohjelmointikielet ja -järjestelmät, jotka käyttävät "aritmeettista pyöristystä" tai "half-up pyöristystä", jossa numero, joka on täsmälleen kahden mahdollisen pyöristetyn arvon välissä, pyöristetään aina ylöspäin. Koodia kääntäessä tai siirtäessä muista kielistä VBA:han, ohjelmoijien on pidettävä mielessä nämä erot välttääkseen hienovaraisia virheitä tai epätarkkuuksia taloudellisissa ja tilastollisissa sovelluksissa.

Vaikka VBA tarjoaa useita toimintoja pyöristykseen, `Ceiling`- ja `Floor`-funktioiden puuttuminen (ilman, että turvaudutaan Excelin WorksheetFunctioniin) korostaa sen natiivien kykyjen rajoitusta. Ohjelmoijat, jotka tulevat ominaisuuksiltaan rikkaammista kielistä, saattavat pitää näitä puutteita epäkäytännöllisinä ja saattavat joutua toteuttamaan mukautettuja ratkaisuja tai sopeuttamaan laskelmansa käyttämään saatavilla olevia toimintoja. Huolimatta näistä rajoituksista, VBA:n pyöristysfunktioiden oikeaoppinen ymmärtäminen ja käyttö voi auttaa varmistamaan, että numeeriset laskelmat ovat sekä tarkkoja että täyttävät useimpien sovellusten vaatimukset.
