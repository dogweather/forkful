---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Muuntaa merkkijono pieniksi kirjaimiksi tarkoittaa muuttamista, jossa kaikki merkkijonon isoja kirjaimia korvataan pienillä kirjaimilla. Ohjelmoijat tekevät tämän usein, kun he haluavat tehdä merkkijonon vertailun herkistämättömäksi.

## Miten:
Go-kielen `strings.ToLower()` -funktiota voidaan käyttää merkkijonon muuttamiseen pieniksi kirjaimiksi. Esimerkiksi:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	var myStr string = "Hei Suomi"
	lowerCaseStr := strings.ToLower(myStr)
	fmt.Println(lowerCaseStr)
}
```

Yllä oleva ohjelma tulostaa: `hei suomi`

## Syvällisemmin:
Historiallinen konteksti: Merkkijonojen muuttaminen pieniksi kirjaimiksi on ollut oleellista tietokoneen ohjelmoinnissa alusta asti yksinkertaistaa tiedon käsittelyä.

Vaihtoehtoja: Go: ssa on myös `strings.EqualFold()`, joka vertaa kahta merkkijonoa herkistämättömästi, ilman että tarvitsee muuntaa merkkijonoja pieniksi kirjaimiksi.

Toteutus yksityiskohdat: `strings.ToLower()` -funktiota toteutettaessa Go käyttää Unicode-määritelmää, joka määrittää, kuinka isot kirjaimet muutetaan pieniksi kirjaimiksi.

## Katso myös:
Lisätietoja ja aiheeseen liittyviä resursseja:
- Go:n virallinen dokumentaatio: [strings.ToLower()](https://pkg.go.dev/strings#ToLower)
- Tutkimus string vertailu Go: [alustavan vertailun taktiikat](https://medium.com/@alexanderravikovich/string-comparison-algorithms-go-1b6463365895)
- Stackoverflow keskustelu: [muita tapoja muuttaa merkkijono alhaiseksi](https://stackoverflow.com/questions/47341278/how-to-change-a-string-to-lowercase-in-golang)