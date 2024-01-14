---
title:    "Swift: Tulostaminen vianjäljitystuloste"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmointiprojektien aikana pelaamme monien vaihtoehtojen ja ratkaisujen kanssa löytääksemme parhaan tavan toteuttaa haluamamme toiminnallisuuden. Debug-tietojen tulostaminen on yksi tehokkaimmista tavoista selvittää, miten koodimme toimii ja mikä aiheuttaa mahdollisia virheitä. Se auttaa meitä paikantamaan ongelmakohtia ja löytämään ratkaisuja.

## Kuinka

Debug-tulosten tulostamiseen on monta tapaa, mutta yleisin ja helppokäyttöisin tapa on käyttää "print()" -toimintoa. Tämä toiminto tulee Swiftin peruskirjastosta ja sen avulla voimme tulostaa muuttujien arvoja, merkkijonoja tai muita tietoja, jotka auttavat meitä ymmärtämään koodin toimintaa.

```Swift
var nimi = "Milla"
var ika = 25

print("Tervetuloa, \(nimi)! Olet \(ika) vuotta vanha.")
```

Tämä koodi tulostaisi seuraavan viestin: "Tervetuloa, Milla! Olet 25 vuotta vanha." Kuten huomaat, voimme käyttää myös muuttujien arvoja osana tulostusta lisäämällä ne viestin sisälle "\()" -merkkien väliin.

## Syvällinen tarkastelu

Print-toiminnon lisäksi Swiftissä on muitakin hyödyllisiä työkaluja debug-tulosten tulostamiseen, kuten "debugPrint()" ja "dump()". DebugPrint-toiminto tulostaa tietoja eri muodoissa, kuten JSON-muodossa, mikä voi olla hyödyllistä tietyissä ohjelmointiprojekteissa. Dump-toiminnon avulla voimme tulostaa tietoja koko koodirakenteesta, joka voi auttaa meitä ymmärtämään koodin suoritusta ja paikantamaan virheitä.

On myös tärkeää huomata, että debug-tulosten tulostamisella voi olla vaikutuksia suorituskykyyn ja esimerkiksi suuret tulostukset voivat hidastaa ohjelmamme toimintaa. Siksi on tärkeää käyttää debug-tulostuksia vain tarpeen mukaan ja poistaa ne lopullisesta koodiversiosta.

## Katso myös

- [Apple:n ohjeet debuggerin ja debug-tulosten tulostamiseen Swiftissä](https://developer.apple.com/library/archive/documentation/ToolsLanguages/Conceptual/Xcode_Overview/DebugYourApp/DebugYourApp.html#//apple_ref/doc/uid/TP40010215-CH55-SW1)
- [Swiftin debug-toiminnon dokumentaatio](https://developer.apple.com/documentation/swift/debugprint)
- [Debuggaus Swiftissä: käyttökelpoisia vinkkejä ja työkaluja](https://www.raywenderlich.com/4721-debugging-in-swift)