---
title:                "Alirivien poiminta"
html_title:           "Swift: Alirivien poiminta"
simple_title:         "Alirivien poiminta"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

Mikä & Miksi?
##
Substringit ovat paloja merkkijonoista, jotka voidaan erottaa ja käyttää erillisenä tietona. Ohjelmoijat voivat tehdä niin, jotta voivat käsitellä ja muokata merkkijonoja tarpeen mukaan.

Miten:
##
Swiftin avulla voit helposti luoda substringeja käyttämällä String-olioita ja niiden indeksejä. Katso esimerkki alla:
```
let merkkijono = "Terve maailma"
let aloitus = merkkijono.index(merkkijono.startIndex, offsetBy: 6)
let loppu = merkkijono.index(merkkijono.endIndex, offsetBy: -1)
let substring = merkkijono[aloitus...loppu]
print(substring)
```
Tulostus: "maailma"

Syväluotaus:
##
Substringien käyttö oli paljon monimutkaisempaa ennen Swiftia, sillä kehittäjien piti itse luoda uusia merkkijonoja ja indeksoida ne. Nykyään Swiftin avulla substringien luominen ja käyttö on selkeämpää ja tehokkaampaa.

Muita tapoja käsitellä merkkijonoja ovat esimerkiksi Regular Expression -mallit ja String-palikat, jotka voivat suorittaa monimutkaisempia operaatioita kuin pelkät substringit. Lisäksi on myös joitakin eroja substringien ja String-palikoiden välillä, joten on tärkeää tietää milloin kumpi on käytettävä.

Katso myös:
##
Jos haluat oppia lisää substringeista ja Swiftiin liittyvistä merkkijonojen käsittelymalleista, voit tutustua seuraaviin lähteisiin:
- [Swiftin viralliset dokumentaatiot](https://developer.apple.com/documentation/swift/strings_and_characters)
- [Hacking with Swift -opas](https://www.hackingwithswift.com/articles/181/how-to-use-string-indexes-and-ranges-in-swift)
- [Regular Expression -mallit](https://www.regular-expressions.info/)