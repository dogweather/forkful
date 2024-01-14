---
title:                "Swift: HtmL-tekstin jäsentäminen"
simple_title:         "HtmL-tekstin jäsentäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi parsia HTML:ää?

HTML on yleisesti käytössä oleva tapa muotoilla ja organisoida verkkosivuja. Kuitenkin, joskus voi olla tarpeellista saada tietoa suoraan sivuston HTML-koodista, esimerkiksi datan keräämiseen tai sivuston sisällön analysointiin. Tässä tapauksessa parsiminen tulee tarpeelliseksi.

## Näin parsit HTML:ää Swiftillä

Parsiminen Swiftillä on suhteellisen helppoa käyttäen Foundation frameworkia ja sen sisältämiä luokkia kuten `Data`ja `String`. Alla esimerkki miten artikkelikuvan alt-tekstit voidaan hakea HTML-koodista:

```Swift
// Oletetaan että htmlData on ladattu jostain ulkoisesta lähteestä
let htmlString = String(data: htmlData, encoding: .utf8)

// Haetaan alt-tekstit ja tallennetaan ne `altTexts` muuttujaan
let regex = try! NSRegularExpression(pattern: "alt=\"(.*?)\"", options: [])
let nsrange = NSMakeRange(0, htmlString.count)
let results = regex.matches(in: htmlString, options: [], range: nsrange)
var altTexts = [String]()
for result in results {
    let range = Range(result.range(at: 1), in: htmlString)!
    altTexts.append(String(htmlString[range]))
}

// Tulostetaan löydetyt alt-tekstit
print(altTexts)
```

Yllä oleva koodi etsii kaikki alt-tekstit sivun HTML-koodista ja tallentaa ne `altTexts` muuttujaan. Tämän lisäksi voit myös muokata koodia lisäämällä halutessasi muita säännöllisiä lausekkeita, kuten esimerkiksi hakemaan sivun linkit tai h1 otsikot.

## Syvällisempi katsaus parsimiseen HTML:ää


Parsiminen Swiftillä on tehokasta käyttäen Foundationin `NSRegularExpression` luokkaa, joka mahdollistaa säännöllisten lausekkeiden käytön merkkijonon muokkaamiseen. Tässä kohtaa on kuitenkin tärkeää huomata, että parsiminen HTML-koodista voi olla haastavaa, jos sivusto muokkaa jatkuvasti rakennettaan tai käyttää dynaamisesti generoituja osia. Lisäksi HTML-koodi voi olla hyvin monimutkaista, mikä voi vaatia tarkempaa säännöllisten lausekkeiden käyttöä.

HTML:n lisäksi voi myös olla tarpeellista parsia muita tiedostomuotoja, kuten JSON tai XML. Tässä tapauksessa tulee tutustua kyseisen tiedoston muotoon ja selvittää mikä on paras tapa löytää tarvitsemasi tiedot.

## Katso myös

- [Foundation framework](https://developer.apple.com/documentation/foundation)
- [Säännölliset lausekkeet Swiftissä](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID412)
- [Sivuston rakenteen muuttuminen](https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags) (Stack Overflow)