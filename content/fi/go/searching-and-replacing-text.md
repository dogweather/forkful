---
title:    "Go: Tekstin etsiminen ja korvaaminen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet ohjelmoija Go-kielellä, saatat löytää itsesi usein etsimässä ja korvaamassa tekstiä koodistasi. Tämä on yleinen tehtävä kehitystyössä, joten on hyödyllistä tietää miten se tehdään tehokkaasti Go:n avulla.

## Miten Tehdä

Go-kielessä on erilaisia tapoja etsiä ja korvata tekstiä, riippuen siitä mitä tarkalleen haluat saavuttaa. Yksi yleisimmistä tavoista on käyttää **strings.Replace** -funktiota, joka korvaa kaikki esiintymät annetulla merkkijonolla annetulla uudella merkkijonolla. Tässä on esimerkkitilanne:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "Tervetuloa Go-ohjelmointimaailmaan"
    newS := strings.Replace(s, "Tervetuloa", "Moi", -1)
    fmt.Println(newS)
}

//Tulostaa "Moi Go-ohjelmointimaailmaan"
```

Huomaa, että **-1** määrittää korvattavien esiintymien lukumäärää. Jos haluat korvata vain tietyn määrän esiintymiä, voit antaa sen korvaavan merkkijonon jälkeen.

## Syväsukellus

On myös muita tapoja käsitellä tekstiä Go-kielellä. Voit esimerkiksi käyttää **strings.Contains** -funktiota tarkistaaksesi, onko merkkijonossa tiettyä tekstiä. Voit myös käyttää **strings.Index** -funktiota löytääksesi tietyn merkkijonon ensimmäisen esiintymän indeksin.

On myös hyödyllistä tietää, että Go tarjoaa myös **regexp** -kirjaston, jolla voit suorittaa monimutkaisempia hakuja ja korvauksia säännöllisiä lausekkeita käyttäen.

## Katso myös

- <a href="https://blog.golang.org/strings">Go:n blogikirjoitus merkkijonojen käsittelystä</a>
- <a href="https://golang.org/pkg/strings/">Go:n virallinen dokumentaatio merkkijonofunktioista</a>
- <a href="https://medium.com/golangspec/regular-expressions-in-go-1-12-1-1-1-29d13cd217dd">Säännöllisten lausekkeiden käyttö Go:ssa</a>