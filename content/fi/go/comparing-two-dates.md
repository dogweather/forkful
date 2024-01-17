---
title:                "Kahden päivämäärän vertailu"
html_title:           "Go: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Päivämäärien vertailu on ohjelmoinnissa tapa verrata kahta erilaista ajanjaksoa toisiinsa ja määrittää niiden välinen suhde. Tämä on tärkeää monissa sovelluksissa, kuten esimerkiksi tietokannoissa, aikatauluissa ja laskureissa. Ohjelmoijat tekevät vertailuja varmistaakseen tietokoneen toiminnan ja oikeiden tulosten saamisen.

## Miten:
```Go
aika1 := time.Date(2020, time.August, 20, 12, 0, 0, 0, time.UTC)
aika2 := time.Date(2021, time.August, 20, 12, 0, 0, 0, time.UTC)

if aika1.Before(aika2) {
    fmt.Println("Ensimmäinen aika on ennen toista aikaa.")
}

if aika1.Equal(aika2) {
    fmt.Println("Ajat ovat samat.")
}
```

Lopputulos:

```Go
Ensimmäinen aika on ennen toista aikaa.
```

## Syvällinen sukellus:
Päivämäärien vertailu on tärkeää myös historiallisessa kontekstissa, sillä eri ohjelmointikielissä voi olla erilaiset tavat käsitellä aikaa ja päivämääriä. Vaihtoehtoisia tapoja vertailuun ovat esimerkiksi Unix-timet, jotka mittaavat aikaa sekunneissa 1. tammikuuta 1970 jälkeen. Go-kielessä taas käytetään aikapisteitä, jotka kattavat laajemman aikajakson.

Päivämäärien vertailuun voi myös vaikuttaa erilaisten aikavyöhykkeiden huomioiminen tai päivämäärämuotojen käyttö.

## Katso myös:
- [Official Go Documentation](https://golang.org/doc/)
- [Timestamp vs Epoch time](https://stackoverflow.com/questions/2422746/epoch-time-vs-timestamp) (englanniksi)
- [Working with Time in Go](https://blog.golang.org/go-time-talk) (englanniksi)
- [Aikapisteet tietokannoissa](https://www.2ndquadrant.com/en/blog/timestamp-vs-timestamptz-vs-timestamp-with-time-zone/) (englanniksi)