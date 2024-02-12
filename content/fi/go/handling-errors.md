---
title:                "Virheiden käsittely"
aliases:
- fi/go/handling-errors.md
date:                  2024-02-03T17:58:26.018827-07:00
model:                 gpt-4-0125-preview
simple_title:         "Virheiden käsittely"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/handling-errors.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Virheiden käsittely Go:ssa tarkoittaa virhetilanteiden tunnistamista ja niihin vastaamista ohjelmassasi. Ohjelmoijat käyttävät virheenkäsittelyä varmistaakseen, että heidän sovelluksensa voivat palautua arvaamattomista tilanteista siististi, mikä johtaa kestävämpään ja luotettavampaan ohjelmistoon.

## Kuinka:

Go:ssa virheenkäsittely hallitaan nimenomaan `error` -tyypin avulla. Funktiot, jotka voivat epäonnistua, palauttavat virheen viimeisenä paluuarvonaan. Tarkistamalla, onko tämä virhearvo `nil`, voit selvittää, onko virhe tapahtunut.

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("arvon on oltava 100 tai vähemmän")
    }
    return value * 2, nil
}

func main() {
    result, err := Compute(150)
    if err != nil {
        fmt.Println("Virhe:", err)
    } else {
        fmt.Println("Tulos:", result)
    }
    
    // Käsittelee virheen siististi
    toinenTulos, toinenVirhe := Compute(50)
    if toinenVirhe != nil {
        fmt.Println("Virhe:", toinenVirhe)
    } else {
        fmt.Println("Tulos:", toinenTulos)
    }
}
```

Esimerkkikoodin tuloste:
```
Virhe: arvon on oltava 100 tai vähemmän
Tulos: 100
```

Tässä esimerkissä `Compute`-funktio palauttaa joko lasketun arvon tai virheen. Kutsuja käsittelee virheen tarkistamalla, onko `err` ei `nil`.

## Syväsukellus

Go:n lähestymistapa virheenkäsittelyyn on tietoisesti suoraviivainen ja tyypiltään turvallinen, vaatien virheiden eksplisiittisen tarkistamisen. Tämä käsite eroaa poikkeusperusteisesta virheenkäsittelystä, jota nähdään kielissä kuten Java ja Python, joissa virheet leviävät kutsupinon ylöspäin, ellei niitä napata poikkeuskäsittelijän toimesta. Go-tiimi argumentoi, että virheiden eksplisiittinen käsittely tuottaa selkeämpää ja luotettavampaa koodia, koska se pakottaa ohjelmoijat kohtaamaan virheet välittömästi siellä, missä ne tapahtuvat.

Kuitenkin jotkut kritiikit mainitsevat, että tämä malli voi johtaa sanalliseen koodiin, erityisesti monimutkaisissa funktioissa, joissa on monia virhealttiita toimintoja. Vastauksena uudemmat Go-versiot ovat tuoneet käyttöön kehittyneempiä virheenkäsittelyominaisuuksia, kuten virheiden käärimisen, mikä tekee virheen kontekstin tarjoamisesta helpompaa menettämättä alkuperäisen virhetiedon. Yhteisö on myös nähnyt ehdotuksia uusista virheenkäsittelymekanismeista, kuten tarkistus/käsittely, vaikkakin nämä ovat vielä keskustelun alla viimeisimmän päivitykseni mukaan.

Go:n virheenkäsittelyfilosofia korostaa virheiden ymmärtämistä ja suunnittelua ohjelman normaalin kulun osana. Tämä lähestymistapa kannustaa kehittämään kestävämpää ja ennustettavampaa ohjelmistoa, vaikkakin potentiaalisesti lisäten boilerplate-koodin määrää. Vaihtoehtoisia malleja ja kirjastoja on olemassa virheenkäsittelyn sujuvoittamiseksi erityisen monimutkaisissa tapauksissa, mutta Go:n sisäänrakennettu `error`-tyyppi pysyy kuitenkin kielen virheenkäsittelyn perustana.
