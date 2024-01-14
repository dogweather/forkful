---
title:    "Kotlin: Kaavamaisen kuvion poistaminen merkeistä"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Kun työskentelet Kotlinin parissa, saatat joutua poistamaan merkkejä, jotka vastaavat tiettyä kaavaa. Tämä voi tulla vastaan esimerkiksi datan käsittelyssä tai tekstien muokkaamisessa. Tässä blogipostissa käymme läpi, miksi tällainen toimenpide voi olla tarpeellinen ja miten se tehdään Kotlinilla.

## Miten

Poistaaksesi merkkejä, jotka vastaavat määrättyä kaavaa, voit käyttää `replace()` metodia `Regex` luokassa. Tässä esimerkissä poistamme kaikki numero-merkit merkkijonosta:

```Kotlin
val merkkijono = "Kotlin on paras ohjelmointikieli 2021!"
val uusiMerkkijono = merkkijono.replace(Regex("\\d"), "")
println(uusiMerkkijono) //Tulostaa: "Kotlin on paras ohjelmointikieli !"
```

Kuten näemme, `replace()` metodi ottaa ensimmäisen parametrinä vastaan halutun kaavan `Regex` tyypin muodossa ja toisena parametrinä uuden merkki- tai merkkijonon, joka korvaa kaavan vastaavat merkit. Voit myös käyttää `replace()` metodia vain poistaaksesi merkkejä ilman korvaamista, kuten teimme edellisessä esimerkissä.

## Syväsukellus

Voit käyttää `Regex` luokkaa poistaaksesi monenlaisia merkkejä riippuen kaavasta, jonka määrität. Voit myös käyttää `Regex` luokkaa suorittaaksesi erilaisia käsittelytoimenpiteitä, kuten merkkijonojen jakamista tai uusien merkkijonojen muodostamista.

Toinen hyödyllinen metodi `Regex` luokassa on `replaceFirst()` metodi, joka poistaa vain ensimmäisen kaavan vastaavan merkin. Tämä on kätevä, jos haluat säilyttää osan merkkijonosta, mutta poistaa vain tietyn merkin tai merkkiluokan.

Kun käytät `Regex` luokkaa, muista ottaa huomioon myös erikoismerkkien, kuten kauttalainojen ja kenoviivojen, käyttö. Voit lukea lisää `Regex` luokasta Kotlinin virallisesta dokumentaatiosta.

## Katso myös

Lue lisää `Regex` luokasta ja sen käytöstä Kotlinissa:
- Kotlinin virallinen dokumentaatio: https://kotlinlang.org/docs/regex.html
- Regex tyypin käyttö esimerkkien kanssa: https://www.baeldung.com/kotlin-regexp
- Regex käytännön esimerkkejä: https://www.infoworld.com/article/3634503/regex-hints-and-tips-for-programmers.html