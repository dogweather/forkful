---
title:                "Alirivien erottaminen"
html_title:           "Kotlin: Alirivien erottaminen"
simple_title:         "Alirivien erottaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien erottaminen on erittäin kätevä toiminto, kun käsittelet merkkijonoja Kotlinissa. Se säästää aikaa ja vaivaa manuaalisessa merkkijonojen leikkaamisessa ja voi auttaa parantamaan koodisi suorituskykyä. Joten jos käytät merkkijonoja ohjelmassasi, substringien erottaminen on tärkeä taito oppia!

## Miten

Voidaksesi erottaa substringejä merkkijonoista Kotlinissa, sinun tarvitsee käyttää sisäänrakennettua `substring()` -funktiota. Se ottaa kaksi parametria: aloituskohdan ja lopetuskohdan, ja palauttaa halutun osan alkuperäisestä merkkijonosta.

```Kotlin
val merkkijono = "Tämä on esimerkkimerkkijono"

// Erota ensimmäinen sana (Tämä)
val ensimmainenSana = merkkijono.substring(0,4)
println(ensimmainenSana) // Tulostaa: Tämä

// Erota toinen sana (on)
val toinenSana = merkkijono.substring(5,7)
println(toinenSana) // Tulostaa: on

// Voit myös antaa vain aloitusindeksin ja jättää lopetuskohdan tyhjäksi,
// jolloin substring erottaa merkkijonon halutusta indeksistä loppuun asti
val loppu = merkkijono.substring(12)
println(loppu) // Tulostaa: esimerkkimerkkijono
```

## Syvällinen sukellus

Substringin erottaminen perustuu merkkijonojen välimuistin käyttöön. Kun kutsut `substring()` -funktiota, se luo uuden merkkijonon, joka sisältää halutun osan alkuperäisestä merkkijonosta. Tämä tarkoittaa sitä, että vaikka merkkijonon leikkaaminen voi vaikuttaa tehokkaalta tavalta käsitellä merkkijonoja, se voi myös aiheuttaa lisätyötä tietokoneellesi, kun se luo useita uusia merkkijonoja muistissa.

On myös tärkeää muistaa, että substringien erottaminen ei muuta alkuperäistä merkkijonoa, vaan palauttaa vain uuden, erillisen merkkijonon. Jos haluat muuttaa alkuperäistä merkkijonoasi, sinun on tallennettava palautettu substring uuteen muuttujaan.

## Katso myös

- [Kotlinin virallinen dokumentaatio merkkijonojen käsittelystä](https://kotlinlang.org/docs/reference/strings.html)
- [Substringien erottaminen JavaScriptissä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)