---
title:                "Koodin refaktorointi"
aliases:
- /fi/swift/refactoring/
date:                  2024-01-26T03:36:56.295531-07:00
model:                 gpt-4-0125-preview
simple_title:         "Koodin refaktorointi"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/refactoring.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Refaktorointi on prosessi, jossa olemassa olevaa tietokonekoodia uudelleenrakennetaan muuttamatta sen ulkoista käyttäytymistä. Ohjelmoijat tekevät sitä siistiäkseen koodikantaa, parantaakseen luettavuutta, ylläpidettävyyttä ja valmistaakseen tietä tuleville ominaisuuksille mahdollisimman vähäisellä teknisellä velalla.

## Kuinka:
Aloitetaan perus Swift-esimerkillä, jossa meillä on toistuvaa koodia:

```Swift
func printUserDetails(firstName: String, lastName: String, age: Int) {
    print("Etunimi: \(firstName)")
    print("Sukunimi: \(lastName)")
    print("Ikä: \(age)")
}

func printUserJob(title: String, company: String) {
    print("Työnimike: \(title)")
    print("Yritys: \(company)")
}
```

Tämän refaktorointiin sisältyisi `User`-rakenteen luominen käyttäjän attribuuttien kapseloimiseksi ja metodin lisääminen tietojen tulostamiseen:

```Swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
    let jobTitle: String
    let company: String

    func printDetails() {
        print("Etunimi: \(firstName)")
        print("Sukunimi: \(lastName)")
        print("Ikä: \(age)")
        print("Työnimike: \(jobTitle)")
        print("Yritys: \(company)")
    }
}

let user = User(firstName: "John", lastName: "Doe", age: 30, jobTitle: "Ohjelmistokehittäjä", company: "Tech Solutions")
user.printDetails()
```

### Näyteulostus:
```
Etunimi: John
Sukunimi: Doe
Ikä: 30
Työnimike: Ohjelmistokehittäjä
Yritys: Tech Solutions
```

## Syväsukellus
Refaktoroinnilla on juuret ohjelmistotekniikan alkuaikoina, mutta termi popularisoitiin 1990-luvun lopulla, erityisesti Martin Fowlerin merkittävän kirjan "Refactoring: Improving the Design of Existing Code" kautta. Kirja esitti periaatteen, että koodia tulisi jatkuvasti siistiä pienin askelin odottamatta erillistä vaihetta.

Vaihtoehtoja manuaaliselle refaktoroinnille sisältävät automatisoidut työkalut ja IDE:t (integroidut kehitysympäristöt), jotka voivat auttaa tunnistamaan duplikaattikoodia, ehdottamaan yksinkertaistuksia ja automaattisesti luomaan osia koodista. Xcode, Swift-kehitystä varten, tarjoaa erilaisia refaktorointityökaluja, kuten nimen muuttaminen ja metodin poiminta -toiminnallisuuden, jotka voivat vähentää inhimillisen virheen mahdollisuutta prosessissa.

Refaktorointia toteutettaessa on tärkeää, että käytössä on vankka testisetti. Testit toimivat turvaverkkona, varmistaen, että tekemäsi muutokset eivät tuo mukanaan bugeja. Tämä on elintärkeää, koska refaktoroinnin päätavoite on muuttaa sisäistä rakennetta vaikuttamatta ulkoiseen käyttäytymiseen.

## Katso myös
- ["Refactoring: Improving the Design of Existing Code" kirjoittanut Martin Fowler](http://martinfowler.com/books/refactoring.html)
- [Swift-dokumentaatio Applen toimesta](https://swift.org/documentation/)
- [Xcode Refaktorointityökalujen käyttö](https://help.apple.com/xcode/mac/current/#/dev91fe7130a)
- [Ray Wenderlichin Swift-tyyliopas](https://github.com/raywenderlich/swift-style-guide)
