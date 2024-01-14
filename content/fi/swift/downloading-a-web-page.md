---
title:                "Swift: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

#### Miksi: Miksi ladata verkkosivu?

Lähes jokainen meistä on joskus joutunut lataamaan verkkosivun, joko työn tai viihteen takia. Tässä artikkelissa opit kuinka saat helposti ladattua verkkosivun käyttäen Swift-ohjelmointikieltä.

#### Miten: Esimerkkejä koodin ja tulosteen kanssa

Jos haluat ladata verkkosivun käyttäen Swift-ohjelmointikieltä, se onnistuu helposti vain muutamalla rivillä koodia. Ensimmäiseksi sinun täytyy tuoda "Foundation" kirjasto, jotta voit käyttää sen sisältämiä luokkia ja metodeja. Sitten voit käyttää "Data(contentsOf: URL)" -metodia lataamaan verkkosivun URL-osoitteen perusteella.

```Swift
import Foundation

if let data = try? Data(contentsOf: URL(string: "https://www.example.com")!) {
    // Sivu ladattu onnistuneesti, voit käsitellä datan tässä
} else {
    // Sivun lataaminen epäonnistui
}
```

Yllä oleva koodi lataa "example.com" -sivun ja tallentaa sen "data" -muuttujaan. Mikäli lataus onnistuu, voit käsitellä saamasi datan haluamallasi tavalla.

#### Syviä vesiä: Tarkempaa tietoa verkkosivun lataamisesta

Verkkosivun lataaminen Swift-ohjelmointikielellä onnistuu käyttäen sen sisäänrakennettua "Foundation" kirjastoa, joka sisältää luokan "Data". Tämän luokan avulla voit ladata suoraan verkkosivun sisällön ilman ylimääräisiä työvaiheita. On kuitenkin tärkeää huomioida, että latauksen tulee tapahtua omassa säikeessään, jotta se ei estä pääsäiettä.

#### Katso myös: Lisää linkkejä

- [Apple:n virallinen dokumentaatio Data-luokasta](https://developer.apple.com/documentation/foundation/data)
- [Swiftin perusteet](https://developer.apple.com/library/archive/documentation/Swift/Conceptual/Swift_Programming_Language/)
- [Hanki lisätietoa lataamisesta linkin takaa](https://www.raywenderlich.com/14172/how-to-write-a-simple-phpmysql-web-service-for-an-ios-app)