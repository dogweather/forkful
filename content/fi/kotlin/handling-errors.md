---
title:                "Virheiden käsittely"
date:                  2024-01-26T00:55:09.012855-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheiden käsittely"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/handling-errors.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Virheiden käsittely on tapa, jolla koodisi käsittelee ongelmia, jotka ilmenevät suorituksen aikana - kuin ottaisi vastaan äkillisen syötön pudottamatta sitä. Ohjelmoijat tekevät sen estääkseen sovelluksen kaatumisen ja tarjotakseen käyttäjille sujuvan kokemuksen.

## Kuinka:
Kotlin tarjoaa `try`, `catch`, `finally` ja `throw` virheiden hallintaan. Tässä on kuinka käytät niitä:

```Kotlin
fun main() {
    val osoittaja = 10
    val nimittäjä = 0

    try {
        val tulos = osoittaja / nimittäjä
        println("Tulos: $tulos")
    } catch (e: ArithmeticException) {
        println("Nollalla ei voi jakaa, kaveri.")
    } finally {
        println("Tämä tapahtuu joka tapauksessa.")
    }
}
```

Tuloste:
```
Nollalla ei voi jakaa, kaveri.
Tämä tapahtuu joka tapauksessa.
```

Jos `try`-lohkossa tapahtuu virhe, suoritus siirtyy `catch`-lohkoon. Se ottaa kiinni tietyn heitotun virheen (tässä tapauksessa `ArithmeticException`). `Finally`-lohko suoritetaan sen jälkeen - riippumatta lopputuloksesta.

## Syväsukellus
`Try-catch`-lohko on ollut olemassa jo varhaisista ohjelmointipäivistä lähtien – se on kuin turvaverkko. Kotlin tarjoaa myös `throw` heittämään poikkeuksen kehiin manuaalisesti, ja `finally` löytyy koodille, joka on pakko suorittaa - usein siivoustyötä.

Vaihtoehtoina ovat `Result`-tyyppi ja Kotlinin `try` lausekkeena.

```Kotlin
val tulos: Result<Int> = try {
    Result.success(osoittaja / nimittäjä)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
Tämä lähestymistapa palauttaa `Result`-olion - saat joko onnistumisen tai epäonnistumisen ilman käsittelemättömän poikkeuksen draamaa.

Totetutus Kotlinissa on siistiä, koska voit käyttää `try` lausekkeena, mikä tarkoittaa, että se palauttaa arvon. Tällaiset valinnat tekevät virheiden käsittelystä Kotlinissa melko monipuolista. Kyse on oikean työkalun valinnasta tehtävään, aivan kuten työpajassa.

## Katso Myös
- Kotlinin dokumentaatio poikkeuksista: [Kotlin Exception Handling](https://kotlinlang.org/docs/exception-handling.html)
- Kotlinin `Result`-tyyppi dokumentaatio: [Kotlin Result](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- Effective Java, 3. painos, kirjoittanut Joshua Bloch - loistavia oivalluksia poikkeuksista, vaikkakin se on Java-spesifi.