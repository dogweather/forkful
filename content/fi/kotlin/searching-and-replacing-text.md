---
title:    "Kotlin: Etsi ja korvaa teksti"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Miksi tekstin etsiminen ja korvaaminen on tärkeää

Joskus ohjelmointiprojektit voivat olla hyvin suuria ja monimutkaisia, ja saattaa olla vaikeaa löytää tiettyä koodinpätkää tai muuttaa sitä. Tekstin etsiminen ja korvaaminen on tärkeä taito, joka auttaa säästämään aikaa ja vaivaa ohjelmoinnissa.

# Kuinka tehdä tekstinhaku ja korvaaminen Kotlinilla

Tekstin etsiminen ja korvaaminen Kotlinilla on helppoa, sillä siinä on valmiina kätevä toiminto tähän tarkoitukseen. Voit käyttää tätä toimintoa String-luokassa ja se koostuu kahdesta osasta: etsimisestä ja korvaamisesta. 

Tässä esimerkissä haemme tekstistä sanaa "tärkeää" ja korvaamme sen sanalla "arvokasta":

```Kotlin
val teksti = "Tekstin etsiminen ja korvaaminen on tärkeää"
println(teksti.replace("tärkeää", "arvokasta"))
```

Tulostus:

```Kotlin
Tekstin etsiminen ja korvaaminen on arvokasta
```

Voit myös käyttää Regex-luokkaa (Regular Expressions) tarkempiin haku- ja korvaustoimintoihin. Tässä esimerkissä muutamme kaikki sanat "työ" sanaksi "projekti" käyttäen Regexiä:

```Kotlin
val teksti = "Työskentelen suuressa ohjelmointiprojektissa"
println(teksti.replace(Regex("työ"), "projekti"))
```

Tulostus:

```Kotlin
Projektilen suuressa ohjelmointiprojektissa
```

# Syvempi sukellus tekstinhakuun ja korvaamiseen

Kotlinin String- ja Regex-luokat tarjoavat paljon erilaisia toimintoja ja vaihtoehtoja tekstinhakuun ja korvaamiseen. Voit esimerkiksi määrittää hakutulokselle erilaisia kohteita, kuten suur- ja pienikirjaimia. Lisäksi voit käyttää monimutkaisempia regexeja, jotka auttavat tarkempien hakujen tekemisessä.

On myös tärkeää huomata, että tekstinhaku ja korvaaminen voivat olla tehokkaita työkaluja myös datan käsittelyssä. Esimerkiksi tekstitiedoston käsittelyssä voit käyttää näitä toimintoja helposti löytääksesi ja korvataksesi tiettyjä sanoja tai fraaseja.

# Katso myös

- [Kotlinin String-luokka](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/)
- [Kotlinin Regex-luokka](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)