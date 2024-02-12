---
title:                "Koodin uudelleenjärjestely"
aliases:
- /fi/kotlin/refactoring/
date:                  2024-01-26T01:43:15.352121-07:00
model:                 gpt-4-0125-preview
simple_title:         "Koodin uudelleenjärjestely"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/refactoring.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Refaktorointi on prosessi, jossa olemassa olevaa koodia muokataan parantamaan sen rakennetta, luettavuutta ja suorituskykyä muuttamatta sen ulkoista käyttäytymistä. Ohjelmoijat refaktoroivat tehdäkseen koodista ylläpidettävämpää, yksinkertaistaakseen uusien ominaisuuksien lisäämistä ja havaitakseen sekä korjatakseen virheitä helpommin.

## Kuinka:
Tässä on Kotlin-koodinpätkä, jossa näytetään yleinen koodin haju ja sen refaktoroitu versio. Aloitamme koodinpalalla, joka tekee liikaa:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Order ID: ${order.id}")
        // Lasketaan tilauksen kokonaissumma
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // Sovelletaan alennusta
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Total: $total")
        // Lisää käsittelyä...
    }
}
```

Refaktoroitu paremman luettavuuden ja vastuunjakautumisen vuoksi:

```kotlin
fun printOrderSummary(order: Order) {
    print("Order ID: ${order.id}")
    val total = calculateTotal(order)
    print("Total: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

Tässä ei ole esimerkkitulostetta, koska emme muuttaneet toiminnallisuutta, mutta koodin luettavuus ja ylläpidettävyys parani huomattavasti!

## Syväsukellus
Refaktorointi konseptina on ollut olemassa ohjelmoinnin alusta lähtien, mutta se todella nousi esiin discipliinina 1990-luvulla, erityisesti Martin Fowlerin julkaiseman "Refactoring: Improving the Design of Existing Code" (1999) jälkeen. Tämä kirja antoi nimen käytännölle ja määritteli järjestäytyneen menetelmän sen soveltamiselle, mukaan lukien katalogin refaktorointitekniikoista.

Vertailtaessa refaktorointia vaihtoehtoihin: voisit kirjoittaa koodin alusta (riskialtista ja aikaa vievää), tai tehdä vain lisäyksiä (johtaa ohjelmiston paisumiseen ja mahdolliseen tekniseen velkaan). Refaktorointi osuu makeaan pisteeseen – se modernisoi ja siistii samalla kun pitää riskit matalina.

Toteutuksen kannalta on olennaista, että sinulla on vankka testien sarja ennen refaktoroinnin aloittamista, jotta et vahingossa muuta ohjelman käyttäytymistä. Monet modernit IDE:t (mukaan lukien IntelliJ Kotlinille) tarjoavat automaattisia refaktorointityökaluja muuttujien nimeämiseen, metodien erotteluun ja muuhun, mikä voi nopeuttaa prosessia ja vähentää virheitä.

## Katso myös
- "Refactoring: Improving the Design of Existing Code" kirjoittanut Martin Fowler (perustavaa työtä tästä aiheesta)
- Kotlin-dokumentaatio koodauskonventioista: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) (ymmärtääkseen 'Kotlin-tavan' puhtaasta koodista)
- JetBrainsin tuki refaktoroinnille IntelliJ IDEA:ssa: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (käytännön refaktorointityökalujen käytöstä)
- Googlen opas laajamittaiseen refaktorointiin: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (näkemyksiä suurempien refaktorointihaasteiden käsittelystä)
