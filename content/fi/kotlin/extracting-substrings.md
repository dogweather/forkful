---
title:                "Kotlin: Irrottamien alimerkkijonojen hakeminen"
simple_title:         "Irrottamien alimerkkijonojen hakeminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien erottelemista käytetään usein silloin, kun halutaan käsitellä vain tietty osa merkkijonosta tai suorittaa toimintoja tietyllä alueella. Tämä voi säästää aikaa ja vaivaa, kun työskennellään merkkijonojen kanssa.

## Kuinka

Merkkijonon lopullinen luettelo muodostuu seuraavista toimenpiteistä:

1. Alusta: Määritä ensin, mistä alkaen haluat ottaa merkkijonon osan.

```Kotlin
val s = "Tämä on esimerkki lauseesta"
val alkuindeksi = 5
```

2. Loppu: Määritä sitten, mihin asti haluat merkkijonon osan ulottuvan.

```Kotlin
val loppuindeksi = 9
```

3. Huomioi, että indeksit alkavat nollasta, joten oikean lopullisen merkkijonon saamiseksi sinun on vähennettävä 1 alku- ja loppuindeksistä.

```Kotlin
val lopullinen = s.substring(alkuindeksi, loppuindeksi - 1)
println(lopullinen)
```

Tämä tuottaa seuraavan tulosteen:

```Kotlin
on e
```

Se on siinä! Olet luonut pienen osan alkuperäisestä merkkijonosta.

## Syvällinen tarkastelu

Jotta ymmärtäisimme paremmin, miten substringien erottelu tapahtuu, on tutkittava hieman syvemmälle. Kun kutsumme [substring()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html) -metodia, se luo uuden merkkijonon alkuperäisestä merkkijonosta käyttämällä annettuja indeksejä. Substringilta otettu merkkijono on siis kopio alkuperäisestä merkkijonosta, ja kaikki siihen tehdyt muutokset eivät vaikuta alkuperäiseen merkkijonoon.

Voit myös käyttää substringia parametrien sijasta vain yhdellä indeksillä, mikä tarkoittaa, että se luo uuden merkkijonon valitsemasta indeksistä loppuun asti.

On myös tärkeää huomata, että substringin indeksit voivat olla negatiivisia, mikä tarkoittaa, että numerointi tapahtuu merkkijonon lopusta. Esimerkiksi indeksi -1 tarkoittaa merkkijonon viimeistä merkkiä ja -3 merkkijonon kolmanneksi viimeistä merkkiä.

## Katso myös

- [Substringin käyttö Kotlinissa](https://www.baeldung.com/kotlin/substring)
- [Kotlinin viralliset dokumentaatiot substringista](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [Merkkijonojen käsittely Kotlinissa](https://www.tutorialkart.com/kotlin/strings-in-kotlin/)

Kiitos lukemisesta ja toivottavasti tämä auttoi sinua ymmärtämään substringien erottelua paremmin. Tehokkaasti koodausta!