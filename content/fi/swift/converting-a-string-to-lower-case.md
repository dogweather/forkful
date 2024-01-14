---
title:    "Swift: Merkkijonon muuttaminen pienaakkosiksi"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoinnissa on tarpeen muuttaa merkkijono pieniksi kirjaimiksi, esimerkiksi tietokannassa tallennetun käyttäjänimen vertailua varten. Tässä blogikirjoituksessa opit, miten voit muuttaa merkkijonon pieniksi kirjaimiksi käyttäen Swift-ohjelmointikieltä.

## Kuinka tehdä se

Swiftissä merkkijonon muuttaminen pieniksi kirjaimiksi tapahtuu `lowercased()`-metodilla. Tämä metodi palauttaa uuden merkkijonon, joka sisältää alkuperäisen merkkijonon pieninä kirjaimina.

```Swift
let nimi = "MARKKU"
print(nimi.lowercased()) // markku
```

Voit myös tallentaa muutetun merkkijonon uuteen muuttujaan, jotta voit käyttää sitä myöhemmin ohjelmassasi.

```Swift
let isoNimi = "MARKKU"
let pieniNimi = isoNimi.lowercased()
print(pieniNimi) // markku
```

## Syvemmälle aiheeseen

Joissakin kielissä on erilaisia aakkosia, jotka voivat aiheuttaa ongelmia vertailtaessa merkkijonoja. Esimerkiksi suomen kielessä on ä, ö ja å-kirjaimet. Onneksi Swiftin `lowercased()`-metodi huomioi nämä erot, joten voit käyttää sitä turvallisesti myös suomen kielellä.

Merkkijonon muuttaminen pieniksi kirjaimiksi on myös tärkeää, jos haluat tehdä hakukyselyitä tietokantaan. Kun molemmat verrattavat arvot ovat samassa muodossa, vertailu on luotettavampi ja tarkempi.

## Katso myös

- [Swiftin virallinen dokumentaatio](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#//apple_ref/doc/uid/TP40014097-CH7-ID291)
- [Stack Overflow - kysymys ja vastaus merkkijonon muuttamisesta pieniksi kirjaimiksi Swiftissä](https://stackoverflow.com/questions/24123320/converting-string-to-lowercase-in-swift)