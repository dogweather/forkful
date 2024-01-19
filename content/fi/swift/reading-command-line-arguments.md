---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Miten Luet Komentorivin Argumentteja Swiftissä?

## Mikä & Miksi?
Lukeminen komentorivin argumentit tarkoittaa sisääntulevien tietojen käsittelyä, kun ohjelmisto suoritetaan komentorivin kautta. Ohjelmoijat tekevät tämän mahdollistaakseen käyttäjän määrittelemään ohjelman toiminnallisuuden suoritushetkellä.

## Näin se toimii:
Tässä on Swift-koodin esimerkki ja siitä saatu näyte tuloste.

```Swift
// Command LineArguments.swift
import Foundation

let arguments = CommandLine.arguments
print("You've entered \(arguments.count) arguments:")
for arg in arguments {
    print(arg)
}
```

Suorita skripti komentorivillä:

```
$ swift CommandLineArguments.swift arg1 arg2 arg3
```

Tuloste:

```
You've entered 4 arguments:
CommandLineArguments.swift
arg1
arg2
arg3
```

## Tarkempaa Tietoa
Komentorivin argumentit on ollut osa ohjelmointikieliä jo pitkään. Ne antavat mahdollisuuden määritellä ohjelman toiminta dynaamisesti suorituksen yhteydessä. Swiftissä `CommandLine.arguments` on taulukko, joka sisältää kaikki komentorivin argumentit. 

Vaihtoehtoisesti voit käyttää `getopt`-funktiota (peräisin C-kielestä) tarkempaan komentorivin argumenttien käsittelyyn. Se antaa enemmän joustavuutta ja kontrollia, mutta vaatii enemmän koodia.

Käytön yksinkertaisuuden ja äärimmäisen suorituskyvyn vuoksi valtaosa Swift-ohjelmoijista suosii `CommandLine.arguments`-lähestymistapaa.

## Lisäluettavaa
1. Apple Developer Documentation: [Command Line Arguments](https://developer.apple.com/documentation/swift/commandline)
2. Medium: [Handling command line arguments in Swift](https://medium.com/@mimicatcodes/handling-command-line-arguments-in-swift-3dd502c821d8)
3. Stack Overflow: [How to read command line arguments in Swift?](https://stackoverflow.com/questions/24035515/how-to-read-command-line-arguments-in-swift)