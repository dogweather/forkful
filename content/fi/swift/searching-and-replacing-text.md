---
title:    "Swift: Tekstin etsiminen ja korvaaminen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi kehittäjät saattavat haluta suorittaa tekstinsyötön hakemista ja korvaamista. Ehkä he haluavat päivittää vanhan koodin uusimmalle Swift-versiolle, tai ehkä he haluavat nopeuttaa textinmuokkausta suurissa projekteissa. Riippumatta syystä, etsiminen ja korvaaminen voi säästää aikaa ja vaivaa ohjelmoinnin aikana.

## Kuinka Tehdä

Ensimmäinen vaihe tekstinsyötön hakemisessa ja korvaamisessa on käyttää *string* -metodia. Tässä esimerkissä etsimme kokonaisen lauseen ja korvaamme sen uudella sanalla.

```Swift
var teksti = "Tervetuloa maailmaan!"
// Tulostaa "Tervetuloa kodinpitäjälle!"
print (teksti.replacingOccurrences (tekstissä: "maailma", joka tapauksessa: "kodinpitäjä"))
```

Tässä esimerkissä käytämme myös *if-statement*-lauseketta tarkistaaksemme, onko etsimäämme tekstiä olemassa. Jos se löytyy, se korvataan, muuten tulostuu alkuperäinen teksti.

```Swift
var teksti = "Tervetuloa maailmaan!"
if teksti.range (of: "uusi maailma") ei ole tyhjä {
    teksti = teksti.replacingOccurrences (of: "maailma", with: "uusi maailma")
}
print (teksti)
```

## Syväitte

On tärkeää ymmärtää, että tekstinsyötön hakeminen ja korvaaminen ei ole rajoitettu vain "string" muuttujiin. Se voi myös toimia muilla tietotyypeillä, kuten *var*, *let*, ja *array*. Näin ollen se voi olla hyödyllinen monissa ohjelmointikohteissa.

Lisäksi, voidaan myös käyttää eri versioita *replacingOccurrences ()* -metodista, kuten *replacingOccurrences (of: with: options:)-versio*, joka mahdollistaa tarkemman valvonnan korvausprosessissa.

## Katso Myös

http://stackoverflow.com/documentation/swift/527/string-manipulation#t=201703071442243159350