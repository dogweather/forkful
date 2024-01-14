---
title:    "Kotlin: Standard errorin kirjoittaminen"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi Kirjoittaa Tavallista Virheenkorjausta

Kirjoittaminen tavalliseen virheenkorjaustilaan (standard error) on tärkeä osa ohjelmointia, ja se auttaa löytämään virheitä tai poikkeuksia, jotka voivat estää ohjelman oikean toiminnan. Se myös auttaa kehittäjiä löytämään ja korjaamaan ongelmia nopeammin, mikä säästää aikaa ja vaivaa.

## Miten Kirjoittaa Tavalliseen Virheenkorjaustilaan

Kotlin-kielen avulla voit helposti kirjoittaa tavalliseen virheenkorjaustilaan käyttämällä funktiota `System.err.println()`.

Käytännön esimerkki:

```
Kotlin funktio:
fun testiFun() {
    System.err.println("Tämä on virheilmoitus")
}

Output:
Tämä on virheilmoitus
```

Tässä esimerkissä olemme luoneet yksinkertaisen testifunktion, joka tulostaa virheilmoituksen tavalliseen virheenkorjaustilaan.

## Syvällisempi Katsaus Kirjoittamiseen Tavalliseen Virheenkorjaustilaan

Kotlin tarjoaa useita työkaluja, jotka auttavat kirjoittamaan tavalliseen virheenkorjaustilaan. Voit esimerkiksi käyttää `System.err.println()` lisäksi myös `System.out.printX` -metodeja, joilla voit tulostaa erilaisia tietotyyppejä tavalliseen virheenkorjaustilaan.

Lisäksi sinun pitäisi aina pyrkiä kirjoittamaan selkeä ja informatiivinen virheilmoitus, joka auttaa löytämään ja korjaamaan ongelman. Voit myös käyttää `try-catch` rakennetta, jolla voit käsitellä ja tulostaa poikkeuksia tavalliseen virheenkorjaustilaan.

## Katso Myös

- [Virheenkorjaus Kotlin-kielellä](https://kotlinlang.org/docs/exceptions.html)
- [Tietoja System.err.println() -metodista](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#err)
- [Kotlin-oppitunti poikkeusten käsittelystä](https://kotlinlang.org/docs/tutorials/exceptions.html)