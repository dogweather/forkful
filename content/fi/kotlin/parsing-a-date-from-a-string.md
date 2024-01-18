---
title:                "Päivämäärän erottaminen merkkijonosta"
html_title:           "Kotlin: Päivämäärän erottaminen merkkijonosta"
simple_title:         "Päivämäärän erottaminen merkkijonosta"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Mitä ja miksi?

Päivämäärän parsiminen merkkijonosta tarkoittaa päivämäärän muuttamista merkkijonoksi, jotta sitä voidaan käsitellä tietokoneohjelmassa. Ohjelmoijat tekevät tätä, koska usein tarvitaan toimintoja, jotka toimivat päivämäärien kanssa ja joskus päivämäärä tulee käyttäjältä merkkijonona.

Miten:

Kotlinin avulla päivämäärän parsiminen merkkijonoksi on helppoa. Alla on esimerkki, jossa aikaleima muunnetaan päivämääräksi ```Kotlin val dateString = "7/1/2021" val date = LocalDate.parse(dateString, DateTimeFormatter.ofPattern("M/d/yyyy")) println(date) ``` Tämä koodi tulostaa "2021-07-01". 

Syvempi sukellus:

Päivämäärän parsiminen merkkijonosta on ollut tärkeä osa ohjelmointia jo pitkään. Aiemmin tämä tehtiin pääasiassa manuaalisesti, mutta nyt on olemassa valmiita funktioita ja kirjastoja, kuten Kotlinin ```LocalDate.parse```, jotka tekevät sen helpommaksi. Vaihtoehtoja päivämäärän muotoilulle on myös useita, esimerkiksi "d/M/yy" tai "yyyy-MM-dd". Tärkeintä on valita sellainen muotoilu, joka sopii parhaiten oman sovelluksen tarpeisiin.

Katso myös:

Lisätietoja päivämäärän parsimisesta merkkijonosta löytyy Kotlinin viralliselta verkkosivustolta sekä eri ohjelmointiyhteisöjen foorumeilta. Hyödyllisiä kirjastoja ja työkaluja ovat esimerkiksi ```DateTimeFormatter``` ja ```SimpleDateFormat```.