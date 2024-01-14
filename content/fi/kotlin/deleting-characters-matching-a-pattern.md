---
title:    "Kotlin: Kaavan mukainen merkkien poistaminen"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi: Miksi joku haluaisi poistaa merkkejä, jotka vastaavat tietytä kaavaa?

Poistaminen merkkejä, jotka vastaavat tietyt kaavaa, voi olla hyödyllistä, kun käsitellään tekstiä, jossa on ylimääräisiä merkkejä tai halutaan poistaa tiettyä tietotyyppiä, kuten numeroita tai erikoismerkkejä.

## Miten tehdä: 

```Kotlin
// Alustetaan teksti muutettavaksi
var teksti = "Tervetuloa tähän ohjelmointiblogiin! Täällä jaamme vinkkejä ja neuvoja erilaisista koodaukseen liittyvistä aiheista. 123#%&."
println(teksti) // Tulostetaan alkuperäinen teksti

// Poistetaan numerot
teksti = teksti.replace(Regex("[0-9]"), "")
println(teksti) // Tulostetaan uusi teksti ilman numeroita

// Poistetaan erikoismerkit
teksti = teksti.replace(Regex("[^A-Za-z ]"), "")
println(teksti) // Tulostetaan uusi teksti ilman erikoismerkkejä
```

Tuloste:

```
Tervetuloa tähän ohjelmointiblogiin! Täällä jaamme vinkkejä ja neuvoja erilaisista koodaukseen liittyvistä aiheista. 123#%&.
Tervetuloa tähän ohjelmointiblogiin! Täällä jaamme vinkkejä ja neuvoja erilaisista koodaukseen liittyvistä aiheista. 
Tervetuloa tähän ohjelmointiblogiin Täällä jaamme vinkkejä ja neuvoja erilaisista koodaukseen liittyvistä aiheista 
```

## Syvemmälle: Poistamisen mekanismi

Kotlinissa `replace()`-funktio ottaa ensimmäisenä parametrinaan `Regex`-tyyppisen muuttujan, joka määrittää poistettavan kaavan. `Regex` voidaan luoda käyttämällä `Regex(pattern)` -konstruktoria, jossa "pattern" on kaava merkkijonona.

Kaavassa voidaan käyttää erilaisia merkintöjä, esimerkiksi `[A-Z]` vastaa kaikkia isoja kirjaimia ja `[^0-9]` vastaa kaikkia, paitsi numeroita. `^` merkki tarkoittaa "ei" tai "not" ja tällä tavalla siihen voidaan määrittää pois poistettavat merkinnät.

## Katso myös:

- [Kotlin Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [String Kotlin Docs](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Regex Tutorial](https://www.regular-expressions.info/tutorial.html)