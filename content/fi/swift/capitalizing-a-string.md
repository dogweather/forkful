---
title:    "Swift: Merkkijonon suurennus"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Miksi

Miksi olisit kiinnostunut muuttamaan merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi? Tässä blogikirjoituksessa kerromme syyn ja näytämme, kuinka voit tehdä sen helposti Swift-ohjelmoinnissa.

# Kuinka tehdä se

Jos haluat muuttaa merkkijonon ensimmäisen kirjaimen isoksi, voit käyttää Swiftin sisäänrakennettua "capitalized" -metodia. Alla olevassa esimerkissä muutamme merkkijonon "swift" isoksi kirjaimeksi ja tulostamme sen konsoliin.

```Swift
let word = "swift"
let capitalizedWord = word.capitalized 
print(capitalizedWord)

// Tulostus: Swift
```

# Syvällinen sukellus

Tämä "capitalized" -metodi perustuu Unicode-säännöstöön, joka määrittelee, miten eri merkkejä käsitellään ja käytetään ohjelmoinnissa. Tässä tapauksessa se tarkoittaa, että ensimmäinen merkki muutetaan isoksi ja muut merkit säilyvät ennallaan.

On myös hyvä huomata, että tämä metodi toimii vain ensimmäisen kirjaimen kanssa, eikä se muuta muita kirjaimia, vaikka ne olisivatkin pienikirjaimisia. Esimerkiksi merkkijono "swiFT" muuttuisi "SwiFT".

# Katso myös

Lisätietoja Unicode-säännöstöstä ja sen käytöstä Swift-ohjelmoinnissa löydät seuraavilta verkkosivuilta:

- [Swiftin virallinen dokumentaatio](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Unicode Consoritumin virallinen sivusto](https://home.unicode.org/)
- [Hacking with Swift -blogikirjoitus aiheesta](https://www.hackingwithswift.com/articles/79/how-to-capitalize-the-first-letter-of-a-string)

Toivottavasti tämä blogikirjoitus auttoi sinua ymmärtämään, miksi ja kuinka voit käyttää "capitalized" -metodia Swiftissä. Kiitos lukemisesta!