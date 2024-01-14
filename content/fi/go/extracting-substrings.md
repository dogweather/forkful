---
title:    "Go: Alimerkkijonojen erottelu"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Monesti ohjelmoinnissa on tarve käsitellä tekstin eri osia, esimerkiksi halutaan poistaa tai korvata tietyt merkkijonot. Tässä tilanteessa substringsien (tekstin osien) eristäminen voi olla hyödyllistä.

## Miten tehdään

Substringsien eristäminen on helppoa Go-ohjelmointikielessä. Käytämme tähän kirjastofunktiota nimeltä `Substr()`.

```Go
// Otetaan esimerkiksi seuraava merkkijono
s := "Tervetuloa Suomeen!"
// Eristetään substrings "Suomeen"
substring := s[11:18]
fmt.Println(substring)
```
Tämä koodinpätkä tulostaa `"Suomeen"`.

## Syvempi sukellus

Substringsien eristäminen perustuu käytännössä vain merkkijonon indeksien käyttämiseen. Indeksi 0 alkaa aina merkkijonon ensimmäisestä merkistä. Esimerkissämme käytimme myös `fmt.Println()` -funktiota, joka tulostaa annetun arvon konsoliin.

## Katso myös

- [Go-kielen virallinen dokumentaatio](https://golang.org/doc/)
- [Käytännöllisiä esimerkkejä Go-kielestä](https://gobyexample.com/)