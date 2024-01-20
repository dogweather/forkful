---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Gleam: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Vaihtamalla merkkijonon pikkukirjaimiksi, voimme muuttaa kaikki tekstin kirjaimet yhtenäiseksi. Tämä auttaa välttämään monenlaisia ​​sekaannuksia ohjelmoinnissa, kuten esimerkiksi identtisten merkkijonjen vertailussa.

## Kuinka:

Swift tarjoaa suoran metodin merkkijonojen muuntamiseen pikkukirjaimiksi, nimeltään 'lowercased()'. Käytämme esimerkkiä havainnollistamaan tätä:

```Swift
let alkuMerkkijono = "Ohjelmointi ON HaUsKAA"
let pieniMerkkijono = alkuMerkkijono.lowercased()

print(pieniMerkkijono)  // Tulostaa: "ohjelmointi on hauskaa"
```
Tämä menetelmä muuntaa JOKAISEN ison kirjaimen merkkijonossa pikkukirjaimeksi.

## Syvemmälle:

Historiallisesti merkkijonon muuntaminen pieniksi kirjaimiksi on ollut olennainen osa ohjelmistokehitystä, mahdollistaen vertailut ja käsittelyn riippumatta alkuperäisestä kirjainkoosta.

Vaihtoehtoisesti, Swift tarjoaa menetelmän 'caseInsensitiveCompare', joka mahdollistaa kirjainkoon huomiotta jättävän vertailun. Tämä ei kuitenkaan muuta merkkijonon alkuperäistä muotoa.

```Swift
let alkuMerkkijono = "Ohjelmointi"
let vertailuMerkkijono = "oHJeLMoInTi"

if alkuMerkkijono.caseInsensitiveCompare(vertailuMerkkijono) == .orderedSame {
    print("Merkkijonot ovat samat!")
} else {
    print("Merkkijonot eivät ole samat!")
}
```
Mutta `lowercased()`-metodia käyttämällä, saamme yhtenäisen muodon, joka helpottaa jatkokäsittelyä.

## Katso myös:

- Swift-koodausoppaat: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- Stack Overflow -keskustelut: https://stackoverflow.com/questions/25111064/swift-convert-string-to-lower-case-and-upper-case 
- Algoritmien opas: https://www.algoexpert.io/blog/convert-string-to-lowercase-swift