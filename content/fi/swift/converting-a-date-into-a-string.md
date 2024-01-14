---
title:    "Swift: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi

Päivämäärän muuttaminen merkkijonoksi voi olla hyödyllistä esimerkiksi, kun haluat näyttää päivämäärän käyttäjälle tai tallentaa sen tiedostoon. Se on myös tärkeää, jos haluat vertailla päivämääriä tai suorittaa muita laskutoimituksia niihin liittyen.

## Miten

Päivämäärän muuttaminen merkkijonoksi Swift-ohjelmointikielessä on helppoa. Voit käyttää DateFormatter-luokkaa muuntaaksesi haluamasi päivämäärämuodon merkkijonoksi. Tässä on yksinkertainen esimerkki, joka tulostaa nykyisen päivämäärän merkkijonona:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy"
let currentDate = dateFormatter.string(from: Date())
print(currentDate)
```

Tämä koodi käyttää dateFormatteria asettaakseen halutun päivämäärämuodon ja sitten muuntaa nykyisen päivämäärän merkkijonoksi. Tulostuksena pitäisi olla jotain tähän tapaan: "04.10.2021".

Voit myös käyttää muita DateFormatterin tarjoamia muotoiluvaihtoehtoja saadaksesi haluamasi päivämäärämuodon, kuten "MMMM yyyy" (lokakuu 2021) tai "EEEE, dd MMMM" (maanantai, 04 lokakuu).

## Syvempi sukellus

DateFormatter-luokan käyttö ei rajoitu vain päivämäärien muuntamiseen merkkijonoiksi. Voit myös käyttää sitä muuntamaan päivämäärän eri aikavyöhykkeeltä toiseen tai luomaan päivämääriä tietystä merkkijonosta. Lisäksi voit käyttää erilaisia kielivaihtoehtoja näyttämään päivämäärän halutulla kielellä.

Jos haluat tutustua tarkemmin DateFormatterin toimintaan, suosittelemme lukemaan sen dokumentaation [täältä](https://developer.apple.com/documentation/foundation/dateformatter). Sieltä löydät täydellisen luettelon muotoilumahdollisuuksista ja lisätietoa luokan käytöstä.

## Katso myös

- [Apple:n esimerkit DateFormatterin käytöstä](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html#//apple_ref/doc/uid/TP40002369-SW1)
- [Swiftin perusteet](https://docs.swift.org/swift-book/GuidedTour/GuidedTour.html)
- [DateFormatter luokan dokumentaatio](https://developer.apple.com/documentation/foundation/dateformatter)