---
title:    "Swift: Kielen Tekstit kirjoittaminen."
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Why: Miksi haluat aloittaa merkkijonon isolla kirjaimella?

On monia mahdollisia syitä capitalizing eli merkkijonon aloitus isolla kirjaimella, kuten ulkoasusta huolehtiminen, tiettyjen ohjelmointistandardien noudattaminen tai käyttäjän syötteen käsittely. Seuraavassa kerromme miten voit tehdä sen kätevästi Swiftillä.

How To: koodiesimerkkien ja tulostusten esittely * ``` Swift ... ``` * koodilohkoissa.

Mitä tahansa syötettä saatkin käyttäjältä, on tärkeää huolehtia että se näyttää hyvältä ja seurata ohjelmointistandardeja. Tässä yksinkertainen koodi, joka ottaa käyttäjän syötteen ja muuttaa sen isoksi kirjaimeksi:

```Swift
let userInput = "hei, mitä kuuluu?"
print(userInput.capitalized) // Tulostaa: Hei, mitä kuuluu?
```

Toinen vaihtoehto on käyttää extensioneja, jotka ovat Swift-kielen tapa lisätä uusia toimintoja olemassa oleviin tietotyyppeihin. Seuraava koodiesimerkki käyttää extensionia String-tietotyypille capitalizing-toiminnon suorittamiseen:

```Swift
extension String {
    func capitalized() -> String {
        return prefix(1).uppercased() + self.lowercased().dropFirst()
    }
}

let userInput = "moi, mitä kuuluu?"
print(userInput.capitalized()) // Tulostaa: Moi, mitä kuuluu?
```

Deep Dive: Syvempää tietoa merkkijonon capitalizingista.

Vaikka yksinkertaisimmassa mallissa, jossa käytettiin vain "capitalized" metodia, tulos näytti hyvältä, se ei välttämättä vastaa kaikkia ohjelmointistandardeja. Esimerkiksi, jos syötteessä on yli yksi sana, sen ensimmäisen kirjaimen pitäisi olla isolla mutta muiden sanojen ei. Tämän takia toisessa koodiesimerkissä käytettiin prefix- ja dropFirst-metodeja varmistaen että vain ensimmäinen sana capitalizoidaan.

Footer (See Also): Katso myös.

- Lisää tietoa String-tietotyypistä: https://developer.apple.com/documentation/swift/string
- Toimintojen lisääminen olemassa oleviin tietotyyppeihin: https://docs.swift.org/swift-book/LanguageGuide/Extensions.html
- Ohjelmointistandardit Swiftissä: https://swift.org/documentation/api-design-guidelines/