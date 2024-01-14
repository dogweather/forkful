---
title:    "Go: Tiedoston kirjoittaminen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi kirjoittaisit tekstitiedoston Go-kielellä. Se voi olla osa isompaa ohjelmaa, jossa tekstitiedoston lukeva ja kirjoittava toiminto on tarpeellinen. Tai ehkä haluat vain harjoitella Go-koodaamista ja tekstitiedoston luominen on hyvä harjoitus.

## Miten

Go-koodilla tekstitiedoston luominen on helppoa. Seuraavaksi näytämme muutaman esimerkin ja tulosteen sen jälkeen.

```Go
// Avaa tiedosto nimeltä "tekstitiedosto.txt" kirjoitusmuodolla
tiedosto, err := os.Create("tekstitiedosto.txt")

// Tarkista olisiko tiedoston luominen aiheuttanut virheen
if err != nil{
    fmt.Println(err)
}

// Kirjoita teksti tiedostoon ja tallenna
tiedosto.WriteString("Tämä on tekstitiedoston sisältö")

// Sulje tiedosto
tiedosto.Close()
```

Tuloste:

Tekstitiedosto luotu! Sen sisältö on: Tämä on tekstitiedoston sisältö.

## Syvällinen sukellus

Tekstitiedoston kirjoittamisessa on tärkeää muistaa, että kirjoitusmuoto määrittelee, kirjoitetaanko teksti tiedoston alkuun vai loppuun. Voit myös käyttää muita kirjoitusmuotoja, kuten `file.WriteString()` tai `file.WriteAt()` riippuen siitä, haluatko kirjoittaa kokonaan uuden tiedoston vai muokata jo olemassa olevaa tiedostoa.

## Katso myös

- [Go-kielean virallinen dokumentaatio](https://golang.org/doc/)
- [Teksti-tiedoston lukeminen Go-kielellä](https://fi.wikipedia.org/wiki/Markdown)