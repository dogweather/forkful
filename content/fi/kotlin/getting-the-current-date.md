---
title:    "Kotlin: Nykyisen päivämäärän saaminen"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Ehkä olet törmännyt olemassa olevaan koodiin, jossa tarvitaan nykyistä päivämäärää, tai ehkä sinulla on oma projekti, johon haluat lisätä toiminnon, jossa käyttäjälle näytetään nykyinen päivämäärä. Joka tapauksessa, koodaaminen nykyisen päivämäärän hakemiseksi voi olla kätevä taito, jonka avulla voit työskennellä päivämäärään liittyvien tehtävien parissa.

## Kuinka tehdä

Kotlinissa on useita erilaisia tapoja saada nykyinen päivämäärä. Yksi yksinkertainen tapa on käyttää standardia `Date`-luokkaa ja `Date()`-funktiota, joka antaa nykyisen ajan ja päivämäärän. Katso alla oleva koodiesimerkki:

```Kotlin
val nykyinenPaivamaara = Date()
println("Nykyinen päivämäärä: $nykyinenPaivamaara")
```
Tämä tulostaa nykyisen päivämäärän ja ajan muodossa "Päivämäärä: Maanantai heinäkuu 19 12:20:49 JST 2021".

Voit myös käyttää `Calendar`-luokkaa saadaksesi enemmän tietoa nykyisestä päivämäärästä, kuten päivän, kuukauden, vuoden jne. Katso alla oleva koodiesimerkki:

```Kotlin
val cal = Calendar.getInstance()
val vuosi = cal.get(Calendar.YEAR)
val kuukausi = cal.get(Calendar.MONTH)
val paiva = cal.get(Calendar.DAY_OF_MONTH)

println("Vuosi: $vuosi")
println("Kuukausi: $kuukausi")
println("Päivä: $paiva")
```

Tämä tulostaa vuoden, kuukauden ja päivän numerot ja näyttää seuraavan tuloksen: "Vuosi: 2021", "Kuukausi: 6" (huomaa, että kuukaudet alkavat nollasta eli kesäkuu on 5.), "Päivä: 19".

## Syvällisempi sukellus

Halutessasi voit tarkentaa vielä enemmän nykyisen päivämäärän hakemiseen. Voit esimerkiksi käyttää seinäkellon aikaan tai muuttaa päivämäärän muotoa. Seuraava koodiesimerkki näyttää kuinka voit tehdä tämän:

```Kotlin
val formatter = DateTimeFormatter.ofPattern("MM/DD/YYYY HH:mm:ss")
val nykyinenAika = LocalDateTime.now()
val formaattattuAika = nykyinenAika.format(formatter)

println("Seinäkellon aika: $nykyinenAika")
println("Muotoiltu aika: $formaatattuAika")
```

Tulostus näyttää nykyisen ajan seinäkellossa ja formaatissa, joka olemme määritelleet. Lisäksi voit käyttää muita `DateTimeFormatter`-luokan metodeja, kuten `ofLocalizedDate` tai `ofLocalizedTime`, jotta voit muuttaa päivämäärän muotoa tavalla, joka vastaa paikallisia asetuksiasi.

## Katso myös

- [Kotlindocs - Date](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date.html)
- [Kotlin Tutorials - Working with Dates and Times in Kotlin](https://kotlinlang.org/docs/datetime.html)
- [Baeldung - Working with Dates in Kotlin](https://www.baeldung.com/kotlin/dates)