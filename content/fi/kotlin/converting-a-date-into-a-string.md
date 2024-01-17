---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "Kotlin: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Päivämäärän muuntaminen merkkijonoksi tarkoittaa päivämäärätiedon muuttamista helpommin ymmärrettävään muotoon. Ohjelmoijat tekevät tämän yleisesti tietojen tallentamisen tai näyttämisen helpottamiseksi.

## Miten: 
Esimerkkejä koodinpätkistä ja tulostus näytetään ```Kotlin ... ``` koodilohkoissa.

Esimerkki 1: Muuntaa päivämäärä ja aikaleima merkkijonoksi. 

```
val date = "21.09.2021"
val time = "12:00"
val stringDate = "$date klo $time"
println(stringDate)
```

Tuloste: 
```
21.09.2021 klo 12:00
```

Esimerkki 2: Käyttäen SimpleDateFormat-luokkaa muuntaaksesi päivämäärän ja aikaleiman haluamassasi muodossa. 

```
val date = Date()
val dateFormat = SimpleDateFormat("dd.MM.yyyy HH:mm")
val stringDate = dateFormat.format(date)
println(stringDate)
```

Tuloste: 
```
21.09.2021 12:10
```

## Syvään sukeltaminen:
Päivämäärän ja aikaleiman muuntamista merkkijonoksi on tehty jo kauan ennen nykyaikaisia ohjelmointikieliä. Yksi vaihtoehto muunnokseen on käyttää Seconds Tiedoston » access stamp, joka tallentaa datan sekunteina. Java SDK tarjoaa myös vaihtoehdon kutsuttuna SimpleDateFormat luokka, jonka avulla voit muuntaa päivämäärän ja aikaleiman haluamassasi muodossa. Kotlin tarjoaa myös samoja luokkia ja metodeja kuin Java.

## Katso myös:
- [Date and Time API in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/)
- [SimpleDateFormat Class in Java](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)