---
title:    "Swift: Komentoriviparametrien lukeminen."
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi lukea komentorivin argumentteja?

Komentorivin argumenttien lukeminen on tärkeä taito, joka auttaa kehittäjiä ohjelman suorittamisen aikana. Se myös sallii ohjelman käyttäjien syöttää muuttujia ohjelman suoritusvaiheessa, mikä tekee ohjelmasta interaktiivisemman ja joustavamman.

## Miten lukea komentorivin argumentteja?

Komentorivin argumenttien lukeminen on yllättävän helppoa Swiftissä. Se voidaan tehdä käyttämällä `CommandLine`-luokkaa ja sen `arguments`-ominaisuutta. Tässä esimerkissä tulostamme kaikki annetut argumentit:

```Swift
let cmdArgs = CommandLine.arguments

for (index, argument) in cmdArgs.enumerated() {
    print("Argumentti \(index+1): \(argument)")
}
```
Esimerkkitulos:
```
Argumentti 1: HelloWorld
Argumentti 2: welcome
```

## Syväsukellus: Komentorivin argumenttien lukeminen

Voit myös määrittää omaa logiikkaa komentorivin argumenttien lukemisen yhteydessä. Tässä esimerkissä luodaan toiminto, joka muuttaa kaikki annetut argumentit isoiksi kirjaimiksi ja tulostaa ne:

```Swift
let cmdArgs = CommandLine.arguments

func convertToUppercase(argument: String) -> String {
    return argument.uppercased()
}

let convertedArgs = cmdArgs.map(convertToUppercase)
print("Muunnetut argumentit: \(convertedArgs)")
```

Esimerkkitulos:
```
Muunnetut argumentit: [HELLOWORLD, WELCOME]
```

## Katso myös

- [Swiftin CommandLine-dokumentaatio](https://developer.apple.com/documentation/swift/commandline)
- [How to Read Command-Line Arguments](https://www.hackingwithswift.com/example-code/system/how-to-read-command-line-arguments-using-swift)