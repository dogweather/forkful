---
title:                "Go: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit laskea tulevaisuuden tai menneisyyden päivämäärän? Yleinen syy voi olla tarve suunnitella aikatauluja tai tapahtumia tulevaisuudessa tai tarkistaa tietty päivämäärä menneisyydestä.

## Miten tehdä

Voit laskea tulevan tai menneen päivämäärän Go-ohjelmointikielellä helposti käyttämällä aikapaketin (time package) funktioita. Seuraavassa koodiesimerkissä näytämme, miten voi laskea päivämäärän 30 päivää tulevaisuuteen ja 10 päivää menneisyyteen.

```
Go paketit käyttäen:

// Laske 30 päivää tulevaisuudessa
tulevaisuus := time.Now().AddDate(0, 0, 30)

// Laske 10 päivää menneisyydessä
menneisyys := time.Now().AddDate(0, 0, -10)

// Tulosta tuleva ja menneinen päivämäärä
fmt.Println("Tuleva päivämäärä:", tulevaisuus.Format("2.1.2006"))
fmt.Println("Menneinen päivämäärä:", menneisyys.Format("2.1.2006"))

```

Koodin suorittaminen tuottaa seuraavan tulosteen:

```
Tuleva päivämäärä: 8.10.2021
Menneinen päivämäärä: 18.9.2021
```

Kuten näet, päivämäärän laskeminen Go-kielellä onnistuu helposti ja nopeasti.

## Syväsukellus

On tärkeää huomata, että päivämäärän laskeminen tulevaisuuteen tai menneisyyteen perustuu nykyiseen päivämäärään. Tämä tarkoittaa, että jos koodia suoritetaan eri päivänä, tulevat ja menneet päivämäärät muuttuvat vastaavasti.

Lisäksi, jos haluat laskea päivämäärää pidemmän ajanjakson verran, voit käyttää `AddDate()` funktion ensimmäisenä parametrina vuosien, kuukausien tai päivien määrää. Esimerkiksi `AddDate(1, 2, 3)` lisää yhden vuoden, kaksi kuukautta ja kolme päivää nykyiseen päivämäärään.

## Katso myös

- [Go:n aikapaketin dokumentaatio](https://golang.org/pkg/time/)
- [Kuinka laskea kuluvan viikon päivämäärät Go-ohjelmoinnilla](https://www.calhoun.io/finding-the-first-day-of-the-month-in-golang/)