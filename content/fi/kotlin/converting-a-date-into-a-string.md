---
title:    "Kotlin: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi: 
Monet ohjelmoijat saattavat kysyä itseltään, miksi olisi hyödyllistä muuntaa päivämäärä merkkijonoksi. Tässä blogikirjoituksessa käymme läpi syitä ja annamme esimerkkejä siitä, miten tämä voidaan tehdä Kotlinilla.

## Miten: 
Muuntaessa päivämäärää merkkijonoksi, tarvitset ensin päivämäärän ja sitten haluamasi muotoilun. Esimerkiksi, jos haluat näyttää päivämäärän muodossa "dd/mm/yyyy", käytä seuraavaa koodia:

```Kotlin
val paivamaara = LocalDate.now()
println(paivamaara.format(DateTimeFormatter.ofPattern("dd/MM/yyyy")))
```
Tämän koodin tuloste riippuu siitä, mikä päivämäärä se suoritetaan. Jos esimerkiksi suoritat tämän koodin tänään, tuloste olisi "21/09/2021".

On myös mahdollista määrittää päivämäärä, jota haluat muuntaa merkkijonoksi, esimerkiksi:

```Kotlin
val syntymapaiva = LocalDate.of(2000, 4, 15)
println(syntymapaiva.format(DateTimeFormatter.ofPattern("dd/MM/yyyy")))
```

Tällöin tuloste olisi "15/04/2000".

## Syvempi sukellus: 
Päivämäärän muuntaminen merkkijonoksi on hyödyllistä esimerkiksi silloin, kun haluat tallentaa päivämäärän tietokantaan tai näyttää sen käyttäjälle jollakin muulla kielellä kuin ohjelman käyttämällä.

Koltinissa on myös muita tapoja muuntaa päivämäärä merkkijonoksi, kuten käyttää valmiita kirjastoja, kuten JodaTimen tai strftime-komentoa. On myös tärkeää huomioida, että päivämäärän muodon merkkijonoksi muuntaminen on tarpeen, jos päivämääräsi sisältää aikaa tai aikavyöhykettä.

## Katso myös: 
- [Java 8 Date and Time API]{https://www.baeldung.com/java-8-date-time-intro}
- [JodaTime]{https://github.com/JodaOrg/joda-time}
- [strftime-komento]{https://strftime.org/}

Toivottavasti tämä kirjoitus auttoi sinua ymmärtämään, miksi ja miten päivämäärän muuntaminen merkkijonoksi voidaan tehdä Kotlinilla. Hyvää ohjelmointia!