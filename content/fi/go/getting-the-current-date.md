---
title:    "Go: Vastaanottaa nykyinen päivämäärä."
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Miksi Päivämäärän Haku Kannattaa

Päivämäärän hakeminen on tärkeä osa monia ohjelmointiprojekteja. Se mahdollistaa ajankohdan tallentamisen ja käsittelemisen, mikä puolestaan tarjoaa käyttömahdollisuuksia esimerkiksi tapahtumien aikajärjestyksen seuraamiseen.

## Kuinka Tehdä Se Go-Ohjelmointikielellä

Go tarjoaa sisäänrakennetun ajanmuutoksen toiminnon nimeltään `time.Now()`. Tämä palauttaa nykyisen ajan ja päivämäärän `time.Time` -muodossa. Seuraavassa esimerkissä tallennetaan paikallinen aika muuttujaan `nykyinenAika` ja tulostetaan se:

```Go
nykyinenAika := time.Now()
fmt.Println(nykyinenAika)
```

Tämä antaa seuraavan tulosteen:

```
2021-08-17 20:30:00.123456789 +0300 MSK m=+0.000000001
```

Voit myös muokata tulosteen ulkoasua esimerkiksi käyttämällä `Format` -toimintoa. Tässä esimerkissä tulostetaan nykyinen aika muodossa "tunti:minuutti":

```Go
nykyinenAika := time.Now()
muokattuAika := nykyinenAika.Format("15:04")
fmt.Println(muokattuAika)
```

Tuloste:

```
20:30
```

## Syventävä Sukellus Päivämäärän Hakuun

Päivämäärän hakeminen voi olla monimutkaisempaa kuin pelkän `time.Now()` -toiminnon käyttö. Esimerkiksi tarvittaessa voit määrittää päivämäärän tarkan aikavyöhykkeen ja aikaleiman. Samoin voit käyttää erilaisia muotoiluvaihtoehtoja tulostetulle päivämäärälle.

Voit löytää lisätietoja ja esimerkkejä Go:n ajanmuutoksen toiminnon käytöstä [Go:n dokumentaatiosta](https://pkg.go.dev/time#Now).

# Katso Myös

- [Go:n dokumentaatio ajanmuutoksen toiminnosta](https://pkg.go.dev/time#Now)
- [Blogikirjoitus Go:n ajanmuutoksen toiminnon käytöstä](https://blog.golang.org/time)
- [Esimerkkejä Go:n ajanmuutoksen toiminnon käytöstä](https://golangdocs.com/go-time-now-get-current-time-in-go)