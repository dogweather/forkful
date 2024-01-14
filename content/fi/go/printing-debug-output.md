---
title:                "Go: Ohjelmointi: Vianmääritystulostuksen tekeminen"
simple_title:         "Ohjelmointi: Vianmääritystulostuksen tekeminen"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Kun ohjelmoimme Go:lla, on usein tarpeen tarkistaa, mitä tapahtuu koodissamme ja miksi se toimii tietyllä tavalla. Tässä tilanteessa on hyödyllistä käyttää debug-ulostulon tulostamista nähdäksemme, mitä tietoja ja arvoja sovelluksemme käsittelee.

## Miten

Käytettäessä Go:n `fmt` -pakettia, meillä on helppo tapa tulostaa debug-merkkijonoja ja arvoja. Voimme käyttää `Printf` -metodia ja interpoloida halutut arvot merkkijonoon.

```Go
package main

import "fmt"

func main() {
  name := "Jenni"
  age := 28
  
  fmt.Printf("Hei, olen %s ja olen %d vuotta vanha", name, age)
}
```

Tämä tulostaisi seuraavan merkkijonon konsolille:

```Go
Hei, olen Jenni ja olen 28 vuotta vanha
```

Voimme myös käyttää `Sprintf` -metodia, jos haluamme tallentaa debug-merkkijonon muuttujaan sen sijaan, että tulostaisimme sen konsolille suoraan.

```Go
package main

import "fmt"

func main() {
  name := "Jenni"
  age := 28
  
  debug := fmt.Sprintf("Hei, olen %s ja olen %d vuotta vanha", name, age)
  fmt.Println(debug)
}
```

Tämä tulostaisi saman merkkijonon kuin edellisessä esimerkissä.

## Syvempi sukellus

`fmt` -paketin lisäksi Go:ssa on myös muita tapoja tulostaa debug-ulostulot. Voimme käyttää `log` -pakettia tai `panic` -funktiota saadaksemme lisätietoja sovelluksemme suorituksesta.

Lisäksi voimme käyttää `reflect` -paketin `ValueOf` -metodia tutkiaksemme rakenteita ja niiden arvoja ohjelmamme sisällä.

## Katso myös

- [Go fmt -paketti](https://golang.org/pkg/fmt/)
- [Go log -paketti](https://golang.org/pkg/log/)
- [Go reflect -paketti](https://golang.org/pkg/reflect/)
- [Go panic -funktio](https://golang.org/pkg/builtin/#panic)