---
title:                "Swift: Lähettäminen http-pyyntö"
simple_title:         "Lähettäminen http-pyyntö"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit lähettää HTTP-pyynnön? Miksi tämä taito on tärkeä? HTTP-pyynnöt ovat olennainen osa modernia web-kehitystä, joten oppimalla niiden lähettämisen voit parantaa kykyjäsi rakentaa tehokkaita ja monipuolisia verkkosovelluksia.

## Kuinka

Yleisin tapa lähettää HTTP-pyyntö on käyttää `URLSession`-luokkaa. Tämä luokka tarjoaa monia hyödyllisiä toimintoja, kuten verkon liikenteen hallinnan ja tietojen lähetyksen. Tässä esimerkissä lähetämme GET-pyynnön ja tulostamme vastauksen:

```Swift
let url = URL(string: "https://example.com")!

URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("Virhe: \(error.localizedDescription)")
    } else if let data = data, let response = response as? HTTPURLResponse {
        print("Vastauskoodi: \(response.statusCode)")
        print("Vastauksen sisältö: \(String(data: data, encoding: .utf8) ?? "Tyhjä")")
    }
}.resume()
```

Tässä koodissa luomme ensin `URL`-olion osoittamaan haluttuun verkkosivuun. Sitten käytämme `URLSession`-luokan `dataTask`-metodia lähettääksemme GET-pyynnön tälle osoitteelle. Sisällä `dataTask`-sulkeessa tarkistamme, onko pyynnössä tapahtunut virheitä ja jos ei, tulostamme vastauksen koodin ja sisällön terminaaliin.

On tärkeää muistaa, että verkkopyyntöjä tulee aina tehdä taustasäikeen kautta, jotta ne eivät häiritse käyttöliittymää. Siksi kutsuttaessa `dataTask`-metodia kutsutaan myös `resume()`-metodia, joka aloittaa pyynnön lähettämisen.

## Syvemmälle

HTTP-pyyntöjen lähettäminen voi tuntua yksinkertaiselta, mutta niiden syvempi ymmärtäminen ja hallitseminen on tärkeää kehittäjälle. Esimerkiksi voit lähettää erilaisia ​​pyyntöjä käyttämällä muita HTTP-metodeja, kuten POST, PUT ja DELETE. Voit myös asettaa haluamasi pyyntöotsakkeet ja lähettää dataa mukana pyynnössä.

On myös tärkeää ymmärtää vastauksen eri koodit, kuten 200 OK, 404 Not Found ja 500 Internal Server Error, ja hallita niitä tarvittaessa. Lisäksi on hyödyllistä tietää, miten käsitellä mahdollisia virheitä ja miten käsitellä vastauksen sisältämää dataa.

HTTP-pyynnön lähettämisen syvällisempi ymmärtäminen auttaa sinua rakentamaan vakaampia ja tehokkaampia verkkosovelluksia.

## Katso myös

- [URLSession Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP Response Status Codes](https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html) (englanniksi)
- [HTTP Methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods) (englanniksi)