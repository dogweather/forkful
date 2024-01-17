---
title:                "Verkkosivun lataaminen"
html_title:           "Go: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tämä artikkeli käsittelee verkkosivujen lataamista Go-kielellä. Verkkosivujen lataaminen tarkoittaa niiden tietojen hakemista ja tallentamista omaan järjestelmään. Monet ohjelmoijat tekevät tätä esimerkiksi netissä olevien tietokantojen ja tiedostojen hyödyntämiseksi.

## Miten:

Voit käyttää `http.Get`-funktiota ladataksesi verkkosivun Go:ssa. Tämä funktio ottaa parametriksi URL-osoitteen ja palauttaa vastauksen ja mahdollisen virheilmoituksen. Esimerkiksi:

```
resp, err := http.Get("https://example.com")
// Tarkista mahdollinen virheilmoitus
if err == nil {
    // Tulosta vastauksen statuskoodi
    fmt.Println("Vastauksen statuskoodi:", resp.StatusCode)
    // Tulosta vastauksen sisältö
    body, _ := ioutil.ReadAll(resp.Body)
    fmt.Println("Sivun sisältö:", string(body))
}
```

Tulostettu sisältö riippuu ladattavan verkkosivun palvelimelta saadusta vastauksesta.

## Syvemmälle:

Go:ssa on useita eri tapoja ladata verkkosivuja, kuten myös muita kirjastoja, kuten `net/http`-kirjastossa käytetty `http.Get`. Voit myös käyttää esimerkiksi kirjastoa nimeltä `net/url` auttamaan URL-osoitteiden muotoilussa ja käsittelyssä. Lisäksi Go:ssa on myös käytettävissä monia muita työkaluja ja kirjastoja verkkosivujen lataamiseen.

## Katso myös:

- [Go:n virallinen verkkosivu](https://golang.org/)
- [net/http-kirjaston dokumentaatio](https://golang.org/pkg/net/http/)
- [net/url-kirjaston dokumentaatio](https://golang.org/pkg/net/url/)