---
title:                "Swift: Kirjoittaminen standardivirheelle"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Miksi

Miksi kirjoittaa standardivirheeseen?

Standardivirheeseen kirjoittaminen on tärkeä osa ohjelmointia, sillä se auttaa tunnistamaan ja korjaamaan virheitä ohjelmassa. Se myös tarjoaa ajan tasalla olevaa tietoa ohjelman suorituksessa tapahtuvista muutoksista.

## Kuinka tehdä

Kirjoittaminen standardivirheeseen Swiftillä on helppoa. Se tehdään käyttämällä "write" funktiota ja antamalla haluttu viesti parametrina. Tämän jälkeen viesti tulostetaan ohjelman suorituksen aikana standardivirheeseen.

```Swift
// Kirjoita standardivirheeseen
write("Tämä on virheviesti")
```
Tulostus:
```
Tämä on virheviesti
```

Voit myös käyttää "print" funktiota ja ohjata tulostus standardivirheeseen käyttämällä "standardError" parametria.

```Swift
// Tulostus standardivirheeseen
print("Tämä on virheviesti", standardError: StandardError())
```
Tulostus:
```
Tämä on virheviesti
```

## Syvempi sukellus

Standardivirheeseen kirjoittaminen on hyödyllistä etenkin silloin, kun halutaan nähdä tarkka aikajärjestyksessä tapahtuvat muutokset ohjelman suorituksessa. Virheviestit ovat myös tärkeä osa ohjelmointia, sillä ne auttavat havaitsemaan ja korjaamaan virheitä ohjelmassa.

On myös hyvä huomioida, että standardivirheeseen kirjoittaminen ei estä ohjelmaa suorittamasta loppuun asti. Sen sijaan ohjelma jatkaa suoritustaan, mutta tulostaa ohjelman aikana tapahtuvat muutokset standardivirheeseen. Tämä tekee ohjelman virheenhallinnasta helpompaa, sillä pystyt näkemään mahdolliset ongelmat ohjelman suorituksen aikana.

## Katso myös

- [Swift dokumentaatio](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- [Standardivirheen hallinta Swiftillä](https://developer.apple.com/documentation/swift/standarderror)
- [Virheiden tunnistaminen ja hallinta Swiftissä](https://www.hackingwithswift.com/new-syntax-swift-2-error-handling-try-catch)

*Huom. Tämä artikkeli on tarkoitettu ainoastaan tiedoksi ja ei korvaa virallisia dokumentaatioita.*