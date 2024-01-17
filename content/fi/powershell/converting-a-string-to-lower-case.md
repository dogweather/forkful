---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "PowerShell: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Muutamalla rivillä selitän mitä merkkijonon muuntaminen pieniksi kirjaimiksi tarkoittaa ja miksi ohjelmoijat tekevät sitä.

Merkkijonon muuttaminen pieniksi kirjaimiksi tarkoittaa, että kaikki merkit merkkijonossa muutetaan pieniksi kirjaimiksi. Tämä on hyödyllistä esimerkiksi kun vertaillaan kahta merkkijonoa, sillä näin voidaan varmistaa, että kirjainkokoeroja ei huomioida.

## Miten:
Tässä näytän esimerkkejä koodista ja annan tulosteen. Koodipätkät ovat ```PowerShell... ``` lohkoissa.

Koodiesimerkki 1: Muunna merkkijono "HELLO WORLD" pieniksi kirjaimiksi.

```PowerShell
$teksti = "HELLO WORLD"
$muunnettu = $teksti.ToLower()
write-host $muunnettu
```

Tuloste:

hello world

Koodiesimerkki 2: Vertaile kahta merkkijonoa ja tarkista merkkijonojen samankaltaisuus.

```PowerShell
$teksti1 = "muuttuuko tämä muotoon pienet kirjaimet"
$teksti2 = "MUUTTUUKO TÄMÄ MUOTOON PIENET KIRJAIMET"
$muunnettu1 = $teksti1.ToLower()
$muunnettu2 = $teksti2.ToLower()
if ($muunnettu1 -eq $muunnettu2){
  write-host "Merkkijonot ovat samanlaiset"
}
else {
  write-host "Merkkijonot eivät ole samanlaiset"
}
```
Tuloste:

Merkkijonot ovat samanlaiset

## Syvennä:
Pienet ja isot kirjaimet ovat osa tietokoneiden käyttämää ASCII-merkistöä, joka sisältää kaikki käytössä olevat merkit ja symbolit. ASCII-merkistöä kehitettiin alun perin tekstipohjaisten laitteiden kuten kirjoituskoneiden ja tulostimien tarpeisiin 1960-luvun lopulla.

Suurin osa nykypäivänä käytetyistä ohjelmointikielistä, myös PowerShell, käyttävät ASCII-merkistöä tietojen tallentamiseen ja käsittelyyn. ASCII-koodit määrittelevät jokaiselle merkille oma numeron, joka kuvaa merkkiä binäärimuodossa.

Toinen tapa muuntaa merkkijono pieniksi kirjaimiksi on käyttää ".ToLower()" -metodia, kuten esimerkeissä näytin. Tämän lisäksi on olemassa myös ".lc()", ".Lowercase()" ja ".Unicode.ToLower()" -metodeja.

## Katso myös:
- [PowerShell String Operations](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-strings?view=powershell-7.1)
- [ASCII Character Set](https://www.computerhope.com/ascii.htm)
- [ASCII versus Unicode](https://www.diffen.com/difference/ASCII_vs_Unicode)