---
title:    "Go: Muuntaminen merkkijonoksi pienin kirjaimin"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi

Konvertoidessa merkkijonoa pienikirjaimiseksi on monia hyödyllisiä käyttötarkoituksia, kuten tiedon käsitteleminen tietokannoissa tai käyttäjän syötteen yhdenmukaistaminen.

## Kuinka

Pienikirjaimiseksi konvertoiminen Go-kielellä on helppoa ja suoraviivaista. Käytä vain sisäänrakennettua `strings.ToLower()` -toimintoa esimerkiksi seuraavasti:

```Go
// Alustetaan muuttuja merkkijonolla
str := "Tämä On Esimerkki"

// Käytetään sisäänrakennettua toimintoa
lower := strings.ToLower(str)

// Tulostetaan lopputulos
fmt.Println(lower)

// Output:
// tämä on esimerkki
```

Voit myös käyttää `strings.ToLower()` yhdessä `bufio` paketin lukijan kanssa, mikä tekee koko käyttökokemuksesta entistäkin helpomman.

## Syvemmälle

Konvertoidessa merkkijonoa pienikirjaimiseksi käytetään yleensä Unicode-standardia, joka huomioi myös muiden kielten ääkköset ja erikoismerkit. Tämä tarkoittaa, että konvertoinnissa käsitellään myös merkkijaon laajennuksia ja niiden oikeaoppista muunnosta.

## Katso Myös

- [Go language official documentation on strings](https://golang.org/pkg/strings/#ToLower)
- [Understanding Unicode in Go](https://medium.com/rungo/string-data-type-in-go-8bc1a532115)
- [Go's bufio package](https://golang.org/pkg/bufio/)