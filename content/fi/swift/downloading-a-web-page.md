---
title:                "Verkkosivun lataaminen"
html_title:           "Swift: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Mikä & miksi?

Lataaminen eli web-sivun hakeminen tarkoittaa verkkosivun sisällön tallentamista tietokoneellesi. Ohjelmoijat lataavat web-sivuja esimerkiksi sovellusten tai tietokantojen luomiseen.

Miten:

`` `Swift
// Luo URL-objekti käyttäen haluttua verkko-osoitetta
let url = URL(string: "https://www.example.com")

// Luo URLSession-objekti lataamista varten
let session = URLSession.shared

// Luo dataTask-objekti ja anna sille URL:n
let dataTask = session.dataTask(with: url!)

// Suorita lataus kutsulla resume()
dataTask.resume()

`` `
Tulostus:

`` `Swift
// Näytä ladattu sisältö konsolissa
if let data = dataTask.data, let dataString = String(data: data, encoding: .utf8) {
    print(dataString)
}

`` `
Tulostus:
"<!DOCTYPE html><html><head><title>Example Website</title></head><body><h1>Welcome to our website!</h1><p>Thank you for visiting.</p></body></html>"


Deep Dive:

Historia: Web-sivujen lataamisen kehitti ensimmäisen kerran Tim Berners-Lee vuonna 1990 käyttämällä HTTP-protokollaa. Nykyään ohjelmoijat voivat käyttää erilaisia kirjastoja ja työkaluja lataamiseen, kuten käyttämämme URLSession.

Vaihtoehdot: Web-sivujen lataamiseen on useita eri vaihtoehtoja, kuten erilaiset kirjastot, kuten Alamofire ja AFNetworking, jotka tarjoavat helpon tavan ladata ja käsitellä web-sivujen sisältöä. Myös selainpohjaiset ohjelmointikielet, kuten JavaScript, voivat ladata ja manipuloida web-sivuja.

Toteutus: Käytettäessä URLSession-objektia, lataaminen tapahtuu taustalla olevalle jonolle. Kun dataTask.resume() kutsutaan, lataus alkaa ja sitä hallitaan automaattisesti. Voit myös käsitellä latausta eri tavoin lisäämällä delegate-ominaisuuksia ja implementoimalla tarvittavia metodeja.

Katso myös:

 - [Apple Developer Documentation: URLSession](https://developer.apple.com/documentation/foundation/urlsession)
 - [Swift.org: URLSession Tutorial](https://swift.org/blog/tutorial-using-urlsession/)
 - [Using URLSession for Networking in Swift](https://medium.com/@jayeshkawli/using-urlsession-for-networking-in-swift-f2a543275720)