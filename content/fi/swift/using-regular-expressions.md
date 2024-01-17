---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Swift: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Mitä ja Miksi?

Regular expressions, eli säännölliset lausekkeet, ovat hyödyllisiä työkaluja, joita ohjelmoijat käyttävät tekstin käsittelyssä. Ne ovat eräänlaisia kuvauksia, joiden avulla voidaan löytää ja manipuloida tiettyjä tekstin muotoja. Niitä käytetään usein esimerkiksi tietojen etsimisessä ja korvaamisessa.

# Miten?

Regular expression -ilmaisuja voi kirjoittaa suoraan koodissa käyttämällä Swiftin sisäänrakennettuja työkaluja. Esimerkiksi seuraava koodinpätkä etsii kaikki merkkijonot, jotka sisältävät sanan "apple" ja tulostaa ne näytölle:

```Swift
let pattern = "apple"
let text = "I love apples and apple pie."
let matches = try! NSRegularExpression(pattern: pattern, options: []).matches(in: text, range: NSRange(text.startIndex..., in: text))

for match in matches {
    let range = Range(match.range, in: text)
    print(text[range!])
}

```

Tulostus:
```
apple
apple
```

# Syvemmälle

Regular expressions kehitettiin alun perin 1950-luvulla matemaatikko Stephen Kleenen työn tuloksena. Ne ovat edelleen tärkeä osa monien ohjelmointikielten ja tekstin käsittelytyökalujen toimintaa. Vaihtoehtoina säännöllisille lausekkeille on esimerkiksi käyttää merkkijonon metodeja, mutta ne eivät välttämättä ole yhtä tehokkaita tai joustavia.

# Katso myös

Voit lukea lisää säännöllisistä lausekkeista esimerkiksi Swiftin virallisesta dokumentaatiosta osoitteessa https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID291

Lisäksi voit löytää erilaisia käyttötapauksia ja harjoituksia säännöllisten lausekkeiden käytölle esimerkiksi sivustolta https://regex101.com/