---
title:    "Go: Joukottelevien merkkijonojen yhdistäminen"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi liittää merkkijonoja yhteen Go-ohjelmointikielessä? Yksi syy siihen on, että se on yksi tapa luoda dynaamisia ja monipuolisia merkkijonoja. Se voi myös auttaa vähentämään koodin toistoa ja tehdä siitä yksinkertaisempaa ja helpommin luettavaa.

## Kuinka tehdä

Go-kielessä merkkijonojen liittäminen yhteen tapahtuu käyttämällä plus-merkkiä (+) tai %-merkkiä. Tässä on esimerkki:

```Go
paketti pää

tuodaan "fmt"

toiminto pää() {
    etuNimi := "Matti"
    sukunimi := "Meikäläinen"

    kokonimi := etuNimi + " " + sukunimi
    fmt.Println("Nimeni on", kokonimi)

    // Output: Nimeni on Matti Meikäläinen
}
```

Toinen tapa liittää merkkijonoja on käyttää %-merkkiä muotoiluehdollisten (format specifiers) kanssa. Tässä on esimerkki:

```Go
laajuus pää

tuodaan "fmt"

toiminto pää() {
    ikä := 25
    viesti := "Olen %v-vuotias"
    viesti = muotoiluviesti, ikä)
    fmt.Println(viesti)

    // Output: Olen 25-vuotias
}
```

## Syvempää tarkastelua

Merkkijonojen liittäminen yhteen käyttäen plus-merkkiä tai %-merkkiä on yleensä tehokkaampaa ja suoraviivaisempaa kuin muita tapoja, kuten buffers (puskurit) tai bytes-paketti. Lisäksi Go-kielessä on olemassa myös muita tapoja liittää monia merkkijonoja yhteen, kuten Join-toiminto ja Sprintf-toiminto.

## Katso myös

- [Go-kielen virallinen dokumentaatio merkkijonojen manipuloinnista](https://golang.org/pkg/strings/)
- [Merkkijonojen käsittely Go-kielessä](https://www.callicoder.com/golang-strings-guide/)
- [Perusteet: Merkkijonot ja merkkijonojen liittäminen](https://www.golangprograms.com/go-language/strings.html)