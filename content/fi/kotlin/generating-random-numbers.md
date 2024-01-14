---
title:    "Kotlin: Satunnaislukujen luominen."
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi käyttää satunnaislukugenerointia?

Satunnaislukugenerointi on hyödyllinen ohjelmoinnin tekniikka, joka mahdollistaa satunnaisen tiedon luomisen tietyn alueen sisällä. Tämä on erityisen hyödyllistä esimerkiksi pelien kehittämisessä, tietoturvan testaamisessa ja simulointiohjelmien luomisessa.

## Miten käyttää satunnaislukugenerointia Kotlinissa?

```Kotlin
fun main() {
    // Satunnaisen kokonaisluvun generointi väliltä 1-10
    val randomInt = (1..10).random()
    println(randomInt)

    // Satunnaisen liukuluvun generointi väliltä 0.0-1.0
    val randomDouble = Math.random()
    println(randomDouble)
}
```

Tässä esimerkissä käytämme Kotlinin sisäistä "random()" funktiota generoimaan satunnaisen luvun. Ensimmäisessä esimerkissä käytämme välin operaattoria "..", joka luo kokonaislukuvälin annettujen lukujen välillä. Toisessa esimerkissä käytämme Java -kirjaston "Math.random()" funktiota, joka generoi satunnaisen liukuluvun väliltä 0.0-1.0.

## Syvempää tietoa satunnaislukugeneroinnista

Satunnaislukugeneroinnissa käytetään usein pseudorandom -algoritmeja, jotka perustuvat matemaattisiin kaavoihin. Tämä tarkoittaa sitä, että vaikka luvut vaikuttavat satunnaisilta, ne voidaan itse asiassa ennustaa. Täydellisen satunnaisuuden saavuttaminen onkin matemaattisesti mahdotonta.

Kotlinin standardikirjasto tarjoaa monia tapoja satunnaislukujen generointiin, kuten "random()" ja "nextInt()". Näiden lisäksi voidaan myös käyttää muita algoritmeja, kuten "SHA1PRNG" algoritmi, joka perustuu satunnaisuuden huonouden estämiseen.

## Katso myös

- [Kotlinin random() dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/) 
- [Java Math -kirjasto](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- [Suosittuja satunnaislukugeneraattoreita](https://en.wikipedia.org/wiki/List_of_random_number_generators)