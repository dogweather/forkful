---
title:                "Kotlin: Virheenkorjaustulostuksen tulostaminen"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Joskus koodin suorituksessa ilmenee ongelmia tai virheitä, joiden löytämiseen ja korjaamiseen tarvitaan lisätietoa. Tulosteiden (debug output) tarkasteleminen on yksi tapa helpottaa ja nopeuttaa tämän prosessin etenemistä.

## Näin teet sen

Tulosteiden tulostaminen eli debuggauksen tekeminen onnistuu Kotlinissa helposti käyttämällä `println()`-funktiota. Tämä tulostaa annetun arvon tai muuttujan sisällön konsoliin.

```Kotlin
val luku = 42
println(luku)
```

Tämän avulla voit tarkastella ohjelman suorituksen aikana eri muuttujien arvoja ja varmistaa, että oikeita arvoja käytetään halutuissa kohdissa.

## Syvemmälle debuggaukseen

Kotlinissa on myös muita apukeinoja debuggaukseen, kuten `assert()`-funktio, joka tarkistaa, että annettu ehto on tosi. Jos ehto ei täyty, ohjelma keskeytetään ja konsoliin tulostetaan ilmoitus virheestä.

```Kotlin
val luku1 = 5
val luku2 = 10
assert(luku1 < luku2, {"Ensimmäisen luvun täytyy olla pienempi kuin toisen!"})
```

Tämä on hyödyllistä esimerkiksi silloin, kun haluat varmistua, että arvoja käytetään oikeassa järjestyksessä tai että ne ovat oikeassa muodossa.

## Katso myös

- [Debugging in Kotlin](https://kotlinlang.org/docs/reference/tools.html#debugging-in-kotlin)
- [Debugging and Troubleshooting in Kotlin](https://blog.kotlin-academy.com/debugging-and-troubleshooting-in-kotlin-6dffcfcbef18)
- [Effective Debugging in Kotlin](https://blog.kotlin-academy.com/effective-debugging-in-kotlin-5ecb07aab87c)