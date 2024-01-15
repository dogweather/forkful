---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Go: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Jos haluat luoda ohjelmassa uusia merkkijonoja, esimerkiksi yhdistää useita merkkijonoja yhdeksi, sinun täytyy käyttää merkkijonon yhdistämistä. Tämä auttaa tekemään koodistasi joustavampaa ja helpottaa merkkijonojen käsittelyä.

## Miten

Yhdistämistä varten voit käyttää sisäänrakennettua "strings" pakettia ja sen "Join" funktiota. Tämä funktio ottaa vastaan kaksi parametria: ensimmäinen on erotinmerkki, jota käytetään yhdistettävien merkkijonojen välissä, ja toinen on merkkijonojen lista, jotka haluat yhdistää.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Yhdistetään kaksi merkkijonoa erotinmerkin avulla
	s := strings.Join([]string{"Tämä on", "yhdistetty", "merkkijono"}, " ")
	fmt.Println(s)
}
```

**Tulos:**

*Tämä on yhdistetty merkkijono*

Voit myös yhdistää merkkijonoja käyttämällä "+=" operaattoria. Tässä tapauksessa merkkijonot ovat peräkkäin ja niitä ei eroteta millään.

```Go
package main

import "fmt"

func main() {
	// Yhdistetään kaksi merkkijonoa käyttäen "+=" operaattoria
	s := "Tämä on" += "yhdistetty" += "merkkijono"
	fmt.Println(s)
}
```

**Tulos:**

*Tämä onyhdistettymerkkijono*

## Syväsukellus

On tärkeää huomata, että merkkijonojen yhdistäminen voi johtaa suorituskykyongelmiin, jos sitä tehdään suuressa mittakaavassa. Tämä johtuu siitä, että jokainen yhdistetty merkkijono luo uuden kopion muistissa.

Tässä tapauksessa on suositeltavaa käyttää "strings.Builder" rakennetta, joka on optimoitu merkkijonojen yhdistämistä varten. Tämä rakenne vähentää muistinkäyttöä ja parantaa suorituskykyä.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Käytetään strings.Builder rakennetta merkkijonojen yhdistämiseen
	var b strings.Builder
	b.WriteString("Tämä")
	b.WriteString("on")
	b.WriteString("yhdistetty")
	b.WriteString("merkkijono")
	fmt.Println(b.String())
}

```

**Tulos:**

*Tämä on yhdistetty merkkijono*

## Katso myös

- Go:n virallinen dokumentaatio merkkijonojen yhdistämisestä: https://golang.org/pkg/strings/#Join
- Hyödyllisiä vinkkejä Go:n opiskeluun: https://github.com/golang/go/wiki/Learn