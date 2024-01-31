---
title:                "Virheenjäljitystulosteiden tulostaminen"
date:                  2024-01-20T17:52:39.867277-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"

category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? / Mitä & Miksi?
Tulostaminen on tapa nähdä ohjelman työstämää dataa livenä. Ohjelmoijat käyttävät tulostusta virheiden etsimiseen ja koodin toiminnan varmistamiseen.

## How to / Kuinka tehdä:
```Go
package main

import (
	"fmt"
	"log"
)

func main() {
	fmt.Println("Hello, Finland!") // Perustulostus

	debugMsg := "Debug-viesti näkyy tässä."
	fmt.Println(debugMsg) // Muuttujan tulostus

	log.Println("Tämä on logitulostus.") // Logitulostus, sisältää ajan
}
```
Käynnistäessäsi ohjelman, tulostus näyttää seuraavalta:
```
Hello, Finland!
Debug-viesti näkyy tässä.
2009/11/10 23:00:00 Tämä on logitulostus.
```

## Deep Dive / Syvä sukellus
Vianjäljitys on vanha konsti; sen juuret ovat aikojen alussa. Debug-tulosteet ovat helppoja, nopeita ja toimivia, mutta raskas virheenjäljitys voi vaatia lisätyökaluja, kuten `gdb` Go:lle. Go:ssa `fmt` ja `log` pakkaukset tarjoavat helpon tavan tulostaa. `fmt` tulostaa suoraan, kun taas `log` lisää aikaleiman ja mahdollisesti tiedostotiedot, mikä on hyödyllistä suuremmissa projekteissa.

## See Also / Katso Myös:
- Go:n dokumentaatio fmt-paketista: https://pkg.go.dev/fmt
- Go:n dokumentaatio log-paketista: https://pkg.go.dev/log
- Blogikirjoitus Go:n vianjäljityksestä: https://blog.golang.org/debugging
- Go:n virallinen vianjäljitystyökalu: https://golang.org/doc/gdb
