---
title:    "Swift: "
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi Extracting Substrings on hyödyllistä

Substringien erottaminen tai "extracting substrings" on tärkeä osa ohjelmointia, joka mahdollistaa tietyn osan merkkijonosta erottamisen ja käytön. Tämä voi olla hyödyllistä esimerkiksi tietojen käsittelyssä, kuten käyttäjän antaman syötteen tarkistamisessa tai tietokannasta tietojen hakemisessa.

## Kuinka Extracting Substrings toimii

Esimerkiksi haluat selvittää käyttäjän antaman nimen ensimmäisen kirjaimen, voit käyttää substring-metodia ja määrittää halutun alueen merkkijonosta:

```Swift
let nimi = "Matti Meikäläinen"
let ensimmäinenKirjain = nimi.substring(to: 1)

// Output: "M"
```

Tässä tapauksessa käyttämällä substring-metodia, pystymme erottamaan halutun alueen merkkijonosta, joka tässä tapauksessa on nimi. Voit myös määrittää, mistä kohdasta alkaen ja kuinka monta merkkiä haluat erotella, kuten seuraavassa esimerkissä:

```Swift
let kaupunki = "Helsinki, Suomi"
let alkukirjaimet = kaupunki.substring(from: 9, length: 2)

// Output: "Su"
```

Tässä esimerkissä olemme määrittäneet alkukirjaimet alkaen kohdasta yhdeksän ja jatkanut kahdella merkillä eteenpäin. Tämä mahdollistaa joustavan tavan erottaa halutut osat merkkijonosta.

## Syvemmälle Extracting Substringsiin

Voit myös käyttää muita metodeja ja ominaisuuksia, kuten components(separatedBy:), joka erottaa merkkijonon annetun merkin tai merkkijonon kohdalta ja palauttaa taulukon erotteluista.

```Swift
let lause = "Tänään on maanantai"
let sanat = lause.components(separatedBy: " ")

// Output: ["Tänään", "on", "maanantai"]
```

Substringien erottaminen on myös hyödyllistä, kun haluat muokata, poistaa tai korvata osia merkkijonosta. Voit käyttää esimerkiksi replaceSubrange-metodia korvaamaan halutun alueen merkkijonosta toisella merkkijonolla.

## Katso myös

- [String - Apple Developer Documentation](https://developer.apple.com/documentation/swift/string)
- [Extracting Substrings - Swift by Sundell](https://www.swiftbysundell.com/basics/substring/)
- [Working with Strings in Swift - Hacking with Swift](https://www.hackingwithswift.com/articles/141/working-with-strings-in-swift)