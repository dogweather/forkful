---
title:    "Go: Merkkijonon suurennus"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi

Miksi olisi hyödyllistä muuttaa merkkijonon kirjainten koko?

## Kuinka tehdä se

Go-ohjelmointikielessä merkkijonon ensimmäisen kirjaimen voi muuttaa isoksi kirjaimeksi käyttämällä funktion `strings.Title()` ja antamalla sille parametrina halutun merkkijonon.

```
Go fmt.Printf("Muutettu merkkijono: %s", strings.Title("ohjelmointi"))
```

Tuloste: Muutettu merkkijono: Ohjelmointi

## Syväkatsaus

Merkkijonon kirjainten koon muuttaminen ei rajoitu pelkästään ensimmäiseen kirjaimeen. Go-kielessä on myös mahdollista muuttaa koko merkkijonon kirjainkoko joko pieneksi tai isoksi käyttäen funktiota `strings.ToLower()` tai `strings.ToUpper()`. Näitä funktioita voisi tarvita vaikkapa silloin, kun halutaan varmistaa, että käyttäjän syöttämä merkkijono on aina tietyn muotoinen.

```
Go fmt.Printf("Muutettu merkkijono: %s", strings.ToLower("OHI ON kOVA SÄÄ"))
```

Tuloste: Muutettu merkkijono: ohi on kova sää

## Katso myös

- [Go-kiehen strings-paketti](https://golang.org/pkg/strings/)
- [Funktiot strings-saantia varten](https://golang.org/pkg/strings/#Title)