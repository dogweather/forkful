---
title:                "Muunna päivämäärä merkkijonoksi."
html_title:           "Swift: Muunna päivämäärä merkkijonoksi."
simple_title:         "Muunna päivämäärä merkkijonoksi."
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?
Kääntäessämme päivämäärän merkkijonoksi, muutamme sen visuaaliseen muotoon, joka voi olla helpompi ymmärtää ja käsitellä ohjelmoinnissa. Tämä on erityisen hyödyllistä esimerkiksi luodessa käyttöliittymän, jossa halutaan näyttää päivämäärä tekstimuodossa. Ohjelmoijina haluamme myös olla joustavia ja tarjota vaihtoehtoisia tapoja esittää päivämäärä eri tilanteissa.

## Kuinka teet sen:
```Swift
let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"
let dateString = formatter.string(from: date)
print(dateString)
```
#### Tulostus: 16.05.2021

Tässä ensin luodaan muuttuja date ja siihen tallennetaan nykyinen päivämäärä. Sitten luodaan DateFormatter-olio, jolla voimme muokata päivämäärän muotoa. Käytämme tässä esimerkissä "dd.MM.yyyy" merkintää, joka tarkoittaa päivän, kuukauden ja vuoden esittämistä numeroina pisteiden välissä. Tämän jälkeen käytämme DateFormatterin string(from: date) metodia, joka muuttaa päivämäärän merkkijonoksi valitsemallamme muotoilulla. Lopuksi tulostelemme muuttujan dateString arvon.

## Syvemmälle:
Historiallisesti käyttäjät ovat tottuneet näkemään päivämäärät tiettyjen sääntöjen mukaisesti, esimerkiksi kuukauden kirjainlyhenne ja numero täyden vuoden sijaan. DateFormatter antaa meille mahdollisuuden luoda omia mukautettuja päivämäärän muotoja. Tämän lisäksi tiettyjen ohjelmointikielten sisään rakennettuja funktioita voidaan käyttää päivämäärää käsittelyyn, kuten esimerkiksi ```DateComponents()```.

## Katso myös:
- DateFormatterin dokumentaatio: https://developer.apple.com/documentation/foundation/dateformatter
- Apple:n ohjeet päivämäärän muotoilusta: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html