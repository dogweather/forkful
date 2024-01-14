---
title:                "Swift: Säännöllisten lausekkeiden käyttö"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Swift-ohjelmoinnissa?

Säännölliset lausekkeet ovat erinomainen työkalu tekstikäsittelyssä ja datan validoinnissa Swift-ohjelmoinnissa. Ne säästävät aikaa ja vaivaa, kun haluat etsiä tiettyjä merkkijonoja tai suorittaa tarkistuksia syöttölomakkeissa.

## Miten käyttää säännöllisiä lausekkeita Swiftissä?

Säännölliset lausekkeet käytetään RegularExpressions-moduulin avulla, joka sisältyy Swiftin Foundation-kirjastoon. Voit luoda RegularExpression-olion antamalla halutun säännöllisen lausekkeen merkkijonona. Tämän jälkeen voit suorittaa erilaisia metodeja tämän olion avulla, kuten `.matches(in:options:range:)` ja `.replaceMatches(in:options:range:with:)`, jotka etsivät vastaavuuksia ja korvaavat ne annetulla merkkijonolla.

```Swift
let pattern = "[a-z]+@[a-z]+\\.com"
let emailRegex = try! NSRegularExpression(pattern: pattern)
let email = "example@email.com"

if emailRegex.matches(email) {
    print("Valid email address")
} else {
    print("Invalid email address")
}

// Output: Valid email address
```

Voit myös käyttää Capturing-ryhmiä säännöllisissä lausekkeissa tallentamaan tietyt osat vastaavasta merkkijonosta. Tämä tehdään lisäämällä säännöllisen lausekkeen sisään sulkeet `(` ja `)` ja antamalla numerot vastaaville ryhmille.

```Swift
let pattern = "(\\d{3})(\\d{3})(\\d{4})"
let phoneRegex = try! NSRegularExpression(pattern: pattern)
let phoneNumber = "1234567890"

if let match = phoneRegex.firstMatch(in: phoneNumber, options: [], range: NSRange(location: 0, length: phoneNumber.utf16.count)) {
    let areaCode = (phoneNumber as NSString).substring(with: match.range(at: 1))
    let middleNumber = (phoneNumber as NSString).substring(with: match.range(at: 2))
    let endNumber = (phoneNumber as NSString).substring(with: match.range(at: 3))
    print("Phone number: (\(areaCode)) \(middleNumber)-\(endNumber)")
}

// Output: Phone number: (123) 456-7890
```

## Syvällisempää tietoa säännöllisistä lausekkeista Swiftissä

Säännöllisten lausekkeiden käyttäminen Swiftissä voi olla erittäin hyödyllistä, mutta niiden opettelu voi vaatia aikaa ja kärsivällisyyttä. On tärkeää ymmärtää säännöllisten lausekkeiden syntaksia ja miten erilaiset erikoismerkit vaikuttavat hakuun. Myös virheiden käsittely ja turvallisuus ovat tärkeitä näkökohtia, kun käytät säännöllisiä lausekkeita ohjelmissasi.

## Katso myös

- [Swiftin RegularExpressions-moduuli](https://developer.apple.com/documentation/foundation/regular_expressions)
- [VapaaCodeCampin opas säännöllisiin lausekkeisiin](https://www.freecodecamp.org/news/an-easy-way-to-learn-regular-expressions-with-examples/)
- [Regexper - säännöllisten lausekkeiden visualisointityökalu](https://regexper.com/)