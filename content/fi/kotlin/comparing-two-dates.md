---
title:    "Kotlin: Tietokoneohjelmoinnin vertailu kahden päivämäärän välillä"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi vertailla päivämääriä?

Vertaamalla päivämääriä voidaan selvittää, kumpi päivämäärä on aikaisempi ja näin tarkastella aikajärjestyksessä tapahtuneita asioita. Tämä voi olla hyödyllistä esimerkiksi tilanteissa, joissa tulee käsitellä dataa, joka on tallennettu eri aikoina.

## Kuinka vertailla päivämääriä?

Vertaileminen onnistuu helposti Kotlinin `Date`-luokan avulla. Ensimmäisenä luodaan kaksi päivämäärää `Date()`-funktiolla, joka ottaa parametreina vuoden, kuukauden ja päivän. Esimerkiksi:

```Kotlin
val date1 = Date(2020, 10, 15)
val date2 = Date(2021, 5, 20)
```

Seuraavaksi voidaan käyttää `compareTo()`-funktiota, joka vertailee päivämääriä ja palauttaa kokonaisluvun sen perusteella, kumman päivämäärän järjestysnumero on suurempi. Positiivinen luku tarkoittaa, että verrattava päivämäärä on aikaisempi, negatiivinen luku taas sitä, että se on myöhäisempi. Tässä esimerkissä vertaillaan luotuja päivämääriä:

```Kotlin
val result = date1.compareTo(date2)
```

`result`-muuttujaan tallentuu nyt arvo `1`, sillä `date1` on aikaisempi päivämäärä kuin `date2`.

Toinen vaihtoehto on käyttää `after()`- ja `before()`-funktioita, jotka palauttavat boolean-arvon sen perusteella, kumpi päivämäärä on aikaisempi. Tässä vertaillaan suoraan luotuja päivämääriä:

```Kotlin
val isAfter = date1.after(date2) // palauttaa arvon false
val isBefore = date1.before(date2) // palauttaa arvon true
```

## Syvempää tietoa vertailemisesta

Päivämäärien vertailu voi olla hieman monimutkaisempaa, sillä päivämääriä tulee usein käsitellä myös aikavyöhykkeiden mukaan. Tässä tapauksessa kannattaa käyttää `Calendar`-luokkaa, joka tarjoaa lisää toiminnallisuutta päivämäärien vertailuun.

`Calendar`-luokassa on käytössä useita metodeja, joilla voi muokata ja vertailla päivämääriä. Esimerkiksi `add()`-funktio lisää halutun ajanjakson päivämäärään ja `get()`-funktio hakee tietyn ajanjakson tiedot, kuten vuoden tai kuukauden. Tässä esimerkissä lisätään päivämäärään kaksi kuukautta ja verrataan sitä alkuperäiseen päivämäärään:

```Kotlin
val cal = Calendar.getInstance()
val originalDate = cal.time // nykyinen päivämäärä
cal.add(Calendar.MONTH, 2) // päivämäärään lisätään kaksi kuukautta
val newDate = cal.time // uusi päivämäärä
val result = originalDate.compareTo(newDate) // palauttaa positiivisen luvun, mikä tarkoittaa että uusi päivämäärä on myöhäisempi
```

On myös hyvä muistaa, että päivämäärät ovat Kotlinissa muokkaamattomia, eli niitä ei voi muuttaa suoraan. Sen sijaan tiettyjen muokkausoperaatioiden jälkeen palautetaan aina uusi päivämäärä-objekti.

##