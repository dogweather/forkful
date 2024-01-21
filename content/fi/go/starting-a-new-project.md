---
title:                "Uuden projektin aloittaminen"
date:                  2024-01-20T18:03:29.678110-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Uuden projektin aloittaminen tarkoittaa uuden sovelluksen tai ohjelman kehityksen käynnistämistä tyhjältä pohjalta. Ohjelmoijat tekevät tämän tuodakseen uusia ideoita eloon ja ratkaistakseen olemassa olevia ongelmia.

## How to: - Näin aloitat:
Perustan luominen uudelle Go-projektille on suoraviivaista. Käytä `go mod` -komennon luomaan uusi moduuli, joka hallinnoi riippuvuuksiasi.

```Go
// Luo uusi hakemisto projektillesi
mkdir myproject
cd myproject

// Alusta uusi moduuli
go mod init myproject

// Luo ensimmäinen tiedosto, esimerkiksi main.go
touch main.go
```

`main.go` tiedostossa:

```Go
package main

import "fmt"

func main() {
    fmt.Println("Tervetuloa uuteen projektiini!")
}

```

Kun ajat `go run main.go`, näet:

```
Tervetuloa uuteen projektiini!
```

## Deep Dive - Syväsukellus:
Go, tai "Golang", on Googlella vuonna 2007 kehitetty kieli. Sen tavoitteena oli yhdistää yksinkertainen syntaksi tehokkaaseen suorituskykyyn, ja se on erityisen suosittu rinnakkaisuuden ja verkko-ohjelmoinnin parissa. Vaihtoehtona Go:n `go mod init` komennolle on vanhempi GOPATH-hakemistopolku, mutta moduulien hallintajärjestelmä on oletuskäytössä Go 1.11:stä lähtien ja on suositeltavampi tämän päivän hankkeissa.

Kun olet luonut moduulin `go mod init` komennolla, Go luo `go.mod` tiedoston, joka määrittelee projektisi riippuvuudet. Saat riippuvuuden lisättyä yksinkertaisesti importtaamalla sen ja ajamalla ohjelmasi. Go lataa puuttuvat riippuvuudet ja päivittää `go.mod` tiedoston automaattisesti.

## See Also - Katso Myös:
- Go:n virallinen dokumentaatio: https://golang.org/doc/
- Moduulien käyttö Go:ssa: https://blog.golang.org/using-go-modules
- Aloittelijan opas Go-kielen maailmaan: https://www.learngoprogramming.com/