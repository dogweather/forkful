---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
date:                  2024-01-20T17:38:17.911188-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Muuttaminen pieniksi kirjaimiksi tarkoittaa merkkijonon kaikkien isojen kirjainten muuttamista pieniksi kirjaimiksi. Koodarit tekevät tämän, jotta voivat vertailla tekstiä ilman, että kirjainkoko vaikuttaa.

## How to:
Go-kielessä merkkijonon muuttaminen pieniksi kirjaimiksi onnistuu `strings`-paketin `ToLower`-funktiolla.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	originalString := "HeI mAAn asUKkaaT!"
	lowerCaseString := strings.ToLower(originalString)
	fmt.Println(lowerCaseString)
}

```

Tämän koodin tuloste on: `hei maan asukkaat!`

## Deep Dive
Go:n vakiostring-kirjaston `strings.ToLower()`-funktio käyttää `unicode`-pakettia, joka käsittää monimutkaisetkin kirjainkoko säännöt maailmanlaajuisesti.

Historiassa on muitakin tapoja muuttaa kirjaimet pieniksi, kuten ASCII-arvojen manipulointi, mutta tämä ei ole yhteensopiva Unicode-merkistön kanssa.

Vaihtoehtoisesti, voit myös käyttää `bytes`-pakettia käsitellessäsi tavuviipaleita, mutta yleisesti `strings.ToLower()` on suoraviivaisin ja tehokkain tapa.

## See Also
- Go dokumentaatio `strings`-paketista: https://pkg.go.dev/strings
- `unicode`-paketti Go:ssa: https://pkg.go.dev/unicode
- Unicode-standardi: http://www.unicode.org/standard/standard.html