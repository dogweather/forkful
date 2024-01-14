---
title:    "Kotlin: Muuntaa merkkijono pieniksi kirjaimiksi"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi

Java-ohjelmoijat ja algoritmin suunnittelijat, jotka haluavat suorittaa merkkijonon muokkauksia, ovat usein kohdanneet tarpeen muuttaa merkkijonon kirjaimet pieniksi kirjaimiksi. Tämä tekniikka auttaa yksinkertaistamaan koodia ja helpottaa kirjainten vertailua, mikä puolestaan johtaa parempaan suorituskykyyn. 

## Miten muuttaa merkkijono pieniksi kirjaimiksi Kotlinissa

```Kotlin 
// Luodaan merkkijono
val sana = "KOTLIN 2021"

// Muutetaan merkkijono pieniksi kirjaimiksi
val pienetKirjaimet = sana.toLowerCase()

// Tulostetaan muutettu merkkijono
println(pienetKirjaimet)

// Tulostaa: kotlin 2021
```

## Syvällinen tutkimus

Kotlinissa on käytettävissä erilaisia ​​tapoja muuttaa merkkijono pienten kirjainten muotoon. Näitä ovat esimerkiksi käyttämällä `toLowerCase()` -metodia, joka on jo käytetty esimerkissämme. Lisäksi on olemassa muita vaihtoehtoisia tapoja käytettävissä, kuten `String.toLowerCase()` ja `Char.toLowerCase()`, jotka molemmat palauttavat muutetun merkkijonon pieniksi kirjaimiksi. 

Lisäksi Kotlin tarjoaa myös mahdollisuuden manipuloida merkkijonoa tietyn alueen sisällä muuttamatta kaikkia merkkejä pieniksi. Tätä varten voidaan käyttää `substring()` ja `toCharArray()` -metodeja. `substring()` -metodi palauttaa osamerkkijonon valitun alueen sisällä, ja `toCharArray()` -metodi muuntaa merkkijonon merkkien taulukoksi. Tämän jälkeen taulukon merkkejä voidaan muuttaa halutulla tavalla, esimerkiksi `Char.toLowerCase()` -metodia käyttämällä.

## Katso myös

- [Kotlin String -virallinen dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/) 
- [Merrijonon muokkaaminen Kotlinissa](https://www.baeldung.com/kotlin/strings)