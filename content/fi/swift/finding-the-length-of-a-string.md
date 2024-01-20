---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkinjonon pituuden löytäminen tarkoittaa merkkien määrän laskemista jonossa. Ohjelmoijat tekevät tämän usein, kun heidän pitää hajottaa, analysoida tai muokata merkkijonoa tavalla tai toisella.

## Miten se tehdään:

Voit laskea merkinjonon pituuden Swiftin `count`-ominaisuudella. Tässä esimerkki:

```Swift
let tervehdys = "Moi, Swiftin ohjelmoijat!"
print(tervehdys.count)
```
Kirjoitettuasi nämä rivit saat tulosteena luvun 26, mikä on `tervehdys`-muuttujan merkkien lukumäärä.

## Syvempi sukellus:

Historiallisesti C-kielisenä koodina merkkijonot päättyivät nollaoktettiin, eikä niitä voitu suoraan laskea. Tämä lisäsi virheen mahdollisuuden.

Swift-kieli tarjoaa `count`-ominaisuuden, joka on tehokkaampi ja turvallisempi tapa laskea merkkijonon pituus. Mutta ole varovainen: `count` palauttaa merkkiyhdistelmien lukumäärän, ei varsinaisesti kirjainten lukumäärää. Esimerkiksi emoji, joka on merkkiyhdistelmä, lasketaan yhdeksi merkiksi.

## Katso myös:

- Swiftin dokumentaatio String-tyypistä: [https://developer.apple.com/documentation/swift/string](https://developer.apple.com/documentation/swift/string)
- Hyödyllinen Swift-oppaan kappale merkkijonojen kanssa työskentelystä: [https://swift.org/documentation](https://swift.org/documentation)