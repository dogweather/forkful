---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstin etsimisessä ja korvaamisessa on kyse merkkijonon tai sisällön löytämisestä tekstistä ja sen muuttamisesta. Ohjelmoijat tekevät sitä, koska se nopeuttaa, hallitsee ja tekee tiedon käsittelystä helpompaa.

## Ohje:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	vanha_teksti := "Tervetuloa Go kielen maailmaan."
	uusi_teksti := strings.Replace(vanha_teksti, "Tervetuloa", "Hei", 1)

	fmt.Println(uusi_teksti)
}
```

Esimerkkilohkon tulos:
```
Hei Go kielen maailmaan.
```
## Syvä sukellus:

### Historiallinen konteksti
Tekstin etsiminen ja korvaaminen ei ole uusi konsepti. Se on ollut ohjelmoinnin perusominaisuuksia jo sen varhaisista päivistä lähtien.

### Vaihtoehtoja
Vaihtoehtoina on monia kirjastoja, jotka tarjoavat erilaisia tapoja etsiä ja korvata tekstiä. Esimerkiksi, `regexp` paketti Go:ssa tarjoaa paljon voimakkaamman tavalla tehdä saman.

### Toteutus yksityiskohdat
`strings.Replace` funktio Go:ssa käyttää laajasti string konkatenointia sisäisesti. Se luo uuden tekstimerkkijonon, joka sisältää korvauksen, sen sijaan, että muuttaisi alkuperäistä merkkijonoa.

## Katso myös:

- [Go:n virallinen dokumentaatio](https://golang.org/pkg/strings/#Replace)
- [Regexp paketti](https://golang.org/pkg/regexp/)
- [Go:n virallinen blogi](https://blog.golang.org/strings)