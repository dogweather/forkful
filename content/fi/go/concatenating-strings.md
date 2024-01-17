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

Mikä & Miksi?
Merkkijonojen yhdistäminen on tapa yhdistää kaksi merkkijonoa yhdeksi. Ohjelmoijat tekevät tätä esimerkiksi, kun he haluavat luoda pidempiä lauseita tai tulostaa tietoja yhtenä kappaleena.

Kuinka tehdä se:
Esimerkki 1:
```Go
package main

import "fmt"

func main() {
    s1 := "Tervetuloa"
    s2 := "kotiin!"
    result := s1 + " " + s2
    fmt.Println(result)
}
```
Tulostus:
Tervetuloa kotiin!

Esimerkki 2:
```Go
package main

import "fmt"

func main() {
    name := "Maija"
    age := 35
    info := "Nimeni on " + name + " ja olen " + string(age) + "-vuotias."
    fmt.Println(info)
}
```
Tulostus:
Nimeni on Maija ja olen 35-vuotias.

Syvällisempi perehtyminen:
Merkkijonojen yhdistämistä on käytetty ohjelmoinnissa jo pitkään. Aiemmin sitä tehtiin usein käyttämällä loogisia operaattoreita, kuten + tai &, mutta nykyään monissa ohjelmointikielissä, kuten Go:ssa, on olemassa valmiita funktioita tai metodeja merkkijonojen yhdistämiseen. Eräs vaihtoehto merkkijonojen yhdistämiseen on käyttää taulukoita ja yhdistää ne yhdeksi merkkijonoksi.

Linkkejä:
Lisätietoja Go:n merkkijonojen yhdistämisestä löytyy osoitteesta https://golang.org/pkg/strings/#Join ja https://golang.org/pkg/bytes/#Buffer.