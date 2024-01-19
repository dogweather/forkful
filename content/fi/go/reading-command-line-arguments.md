---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Komentoriviparametrien lukeminen on prosessi, jossa ohjelma ottaa käyttäjältä tulevan syötteen suoraan komentoriviltä. Ohjelmoijat tekevät tämän käyttäjälsiirtämättämän toiminnan ja joustavuuden vuoksi.

## Kuinka:

Voit lukea komentoriviparametrit Go:lla `os`-paketin `Args`-toimintoa käyttäen.

```Go 
package main

import (
	"fmt"
	"os"
)

func main() {
	argsWithoutProg := os.Args[1:]
	argsWithProg := os.Args

	fmt.Println(argsWithoutProg)
	fmt.Println(argsWithProg)
}
```
Näyte tuloste:
```
$ go run main.go arg1 arg2 arg3 
[arg1 arg2 arg3]
[main.go arg1 arg2 arg3]
```

## Syvällisemmin:

Komentoriviparametrin vastaanottaminen alkoi jo vanhoissa tekstipohjaisissa käyttöjärjestelmissä, kuten UNIX:issa. Se antaa ohjelmoijille mahdollisuuden antaa syötteitä scripteille tai ohjelmille suoraan komentoriviltä.

Go:n `os.Args` tarjoaa suoran pääsyn näihin argumentteihin. Ensimmäinen arvo (os.Args[0]) on ohjelman nimi, ja seuraavat arvot (os.Args[1:], os.Args[2:], jne.) ovat argumentteja, jotka on annettu ohjelmalle.

Vaihtoehtoisesti voit käyttää myös `flag`-pakettia, jos tarvitset kehittyneempiä komentoriviparametrien käsittelyominaisuuksia, kuten lippuja tai kytkimiä.

## Katso myös:

1. Os-paketti Go:n dokumentaatiossa: https://pkg.go.dev/os
2. Komentoriviparametrit Go:ssa: https://gobyexample.com/command-line-arguments
3. Käyttöohjeet flag-pakettiin: https://golang.org/pkg/flag/