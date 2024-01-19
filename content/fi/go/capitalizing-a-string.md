---
title:                "Merkkijonon suuraakkostaminen"
html_title:           "Go: Merkkijonon suuraakkostaminen"
simple_title:         "Merkkijonon suuraakkostaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonon isoksi muuttaminen tarkoittaa jokaisen merkin muuttamista isoksi kirjaimiksi. Ohjelmoijat tekevät tämän siksi, että järjestelmät voivat tulkita merkkijonot herkästi kirjainkoosta riippuen.

## Näin teet:

Go:n tarjoamassa `strings` kirjastossa on funktio `ToUpper()`, jolla voit muuttaa merkkijonon isoksi. Tässä on esimerkki sen käyttömisestä:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	s := "hei maailma"
	fmt.Println(strings.ToUpper(s))
}
```
Tämä tulostaa:
```
HEI MAAILMA
```

## Syvällisempi tieto:

Historia: Merkkijonon isoksi muuttamisen käyttö juontaa juurensa aikaan, jolloin tietokonejärjestelmät ja ohjelmat olivat suuria herkkiä kirjainkoosta, esimerkiksi käyttäjätunnukset ja salasanat.

Vaihtoehdot: Voit myös muuttaa merkkijonon pelkästään ensimmäisen kirjaimen isoksi käyttäen `Title()` funktiota samassa `strings` kirjastossa.

Yksityiskohdat: Go:n `ToUpper` funktio käy läpi merkkijonon ja muuttaa jokaisen pienaakkosen isoakseliksi käyttäen Unicode:n yleisiä töyssyjä määritellä, mikä merkkijono muuttuu isoksi.

## Katso myös:

Muita liittyviä funktioita Go:n `strings` kirjastossa - https://pkg.go.dev/strings.
Tietoa Unicode säännöstöstä - https://www.unicode.org/standard/standard.html.