---
title:    "Swift: Stringien yhdistäminen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

Onko Swift-ohjelmoija, oletko koskaan törmännyt tarpeeseen yhdistää merkkijonoja koodissasi? Tässä blogikirjoituksessa jaamme kanssasi muutamia vinkkejä ja syvempää tietoa kuinka voit yhdistää merkkijonoja tehokkaasti Swiftillä. Lue ja opi lisää!

##Miksi

Merkkijonojen yhdistäminen (concatenation) on tärkeä osa ohjelmointia, sillä usein tarvitsemme luoda uusi merkkijono yhdistämällä olemassa olevia merkkijonoja. Tämä voi olla esimerkiksi käyttäjän syöttämä nimi ja sukunimi, joita tarvitsemme yhdistää yhdeksi merkkijonoksi.

##Kuinka tehdä se

Voit yhdistää merkkijonoja muutamalla eri tavalla Swiftillä. Yksinkertaisin tapa on käyttää "+" operaattoria, joka yhdistää kaksi merkkijonoa yhdeksi. Esimerkiksi:

```Swift
let nimi = "Matti"
let sukunimi = "Meikäläinen"
let kokonimi = nimi + " " + sukunimi

print(kokonimi) //Tulostaa "Matti Meikäläinen"
```

Voit myös yhdistää useampia merkkijonoja käyttämällä join() metodia, joka ottaa vastaan merkkijonojen taulukon ja yhdistää ne yhdeksi merkkijonoksi. Esimerkiksi:

```Swift
let sanat = ["Hei", "maailma", "!"]
let lause = sanat.join(separator: " ")

print(lause) //Tulostaa "Hei maailma !"
```

##Syvällisempiä tietoja

Yhdistäminen (+) ja join() ovat käteviä tapoja yhdistää merkkijonoja, mutta on myös hyvä tietää, että nämä metodit luovat jokaisen kerran uuden merkkijonon. Jos sinun tarvitsee yhdistää suuri määrä merkkijonoja, tämä voi vaikuttaa suorituskykyyn.

Sen sijaan voit käyttää StringInterpolation muodostimia, jotka ovat nopeampia ja tehokkaampia. Esimerkiksi:

```Swift
let nimi = "Matti"
let sukunimi = "Meikäläinen"
let kokonimi = "\(nimi) \(sukunimi)"

print(kokonimi) //Tulostaa "Matti Meikäläinen"
```

Näiden muodostimien avulla säästät muistia ja suoritusaikaa, sillä ne eivät luo joka kerta uutta merkkijonoa.

##Katso myös

Haluatko oppia lisää merkkijonojen käsittelystä Swiftillä? Tutustu seuraaviin hyödyllisiin artikkeleihin:

- [How to Use String Interpolation in Swift](https://www.hackingwithswift.com/read/0/2/string-interpolation)
- [Concatenating Strings in Swift](https://medium.com/@alexandre_bernard/concatenating-strings-in-swift-b898b8fd66b3)
- [Manipulating Strings in Swift](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html)

Toivottavasti tämä artikkeli auttoi sinua ymmärtämään miten voit yhdistää merkkijonoja Swiftissä. Jaa tämä artikkeli myös ystävillesi, jotka saattavat tarvita apua merkkijonojen yhdistämisessä. Kiitos lukemisesta ja nähdään seuraavassa blogikirjoituksessa!