---
title:                "Komennoriviparametrien lukeminen"
date:                  2024-01-20T17:56:09.242375-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?  
Mikä ja miksi?

Komennon rivin argumentit ovat ohjelmalle annettuja syötteitä sen käynnistyksen yhteydessä. Ne mahdollistavat ohjelman suoritustavan räätälöinnin ja erilaisten käyttöskenaarioiden käsittelyn suoraan komentoriviltä.

## How to:  
Kuinka:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Hae kaikki komentorivin argumentit
	args := os.Args[1:]

	// Tulosta argumentit
	for i, arg := range args {
		fmt.Printf("Argument %d: %s\n", i+1, arg)
	}
}
```

Kun tallennat yllä olevan koodin tiedostoon ja ajat sen komennolla `go run yourfile.go arg1 arg2`, saat tulosteen:

```
Argument 1: arg1
Argument 2: arg2
```

## Deep Dive  
Syväsukellus:

Komentorivin argumenttien lukeminen on ollut juttu käytännössä niin kauan kuin käyttöjärjestelmät on olemassa. Go:ssa `os.Args` taulukko tarjoaa suoran pääsyn näihin argumentteihin. `os.Args[0]` on ohjelman nimi ja `os.Args[1:]` ovat käyttäjän syöttämät argumentit.

Vaihtoehtoina `os.Args`:ille ovat flag-kirjasto yksinkertaisemmille tarpeille tai voimakkaampi kolmannen osapuolen kirjastoja, kuten `cobra` tai `urfave/cli`, monimutkaisempia komennollinen argumenteja varten.
Toteutuksessa tärkeää on muistaa, että ohjelmalle annetut argumentit ovat aina merkkijonoja, joten ne tulee muuntaa haluttuun tietotyyppiin ennen käyttöä.

## See Also  
Katso myös:

- Go:n standardikirjaston dokumentaatio `os` paketista: https://pkg.go.dev/os
- `flag` paketin dokumentaatio: https://pkg.go.dev/flag
- `cobra`-kirjasto: https://github.com/spf13/cobra
- `urfave/cli`-kirjasto: https://github.com/urfave/cli
