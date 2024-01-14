---
title:    "Kotlin: Aliljonojen erottaminen"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi
Monissa ohjelmointitehtävissä on tarve erottaa osa merkkijonosta ja käsitellä sitä erikseen. Tätä varten Kotlinissa on käytettävissä substring-metodi. Se on hyödyllinen työkalu merkkijonojen käsittelyssä ja voi säästää aikaa ja vaivaa.

## Kuinka
```Kotlin 
val sana = "Tervetuloa"
val osaSanaa = sana.substring(0,5)
println(osaSanaa)
```
Tämä tulostaa "Terve", joka on alkuperäisestä sanasta ensimmäiset viisi kirjainta.

Voimme myös käyttää startIndex-parametria ja jättää lopetusindeksin antamatta, jolloin metodi palauttaa kaikki kirjaimet annetusta indeksistä loppuun asti.
```Kotlin
val sana = "Moi maailma!"
val osaSanaa = sana.substring(4)
println(osaSanaa)
```
Tämä tulostaa "maailma!".

Substring-metodilla on myös toinen versio, jossa sen sijaan että annamme indeksit, voimme antaa sille range-of-indeksit käyttämällä rangeTo()-funktiota.
```Kotlin
val sana = "Tämä lause on tarkoitettu sinulle."
val osaSanaa = sana.substring(IntRange(5,9))
println(osaSanaa)
```
Tämä tulostaa "lause".

## Syväsyvennys
Substring-metodi palauttaa aina uuden merkkijonon eikä muuta alkuperäistä merkkijonoa. Se myös toimii vain, jos annettu range tai indeksit ovat merkkijonon sisällä. Jos yritämme antaa liian suuren lopetusindeksin, se palauttaa joko lopun merkkijonosta tai antaa virheen.

Substringin voi myös yhdistää muihin merkkijonon käsittelymetodeihin, kuten trim(), replace() ja contains(). Tämä voi olla hyödyllistä, jos haluamme esimerkiksi poistaa osan merkkijonosta tai tarkistaa, sisältääkö se tietyn osajonon.

## Katso myös
- [official documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring.html)
- [Kotlin String Cheat Sheet](https://medium.com/@BladeCoder/kotlin-single-line-and-multi-line-strings-33d8b7b2f8cf)
- [Substring vs SubSequence in Kotlin](https://medium.com/@psyanite/kotlin-substring-vs-subsequence-3ba9adae1dbf)