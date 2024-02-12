---
title:                "Assosiatiivisten taulukoiden käyttö"
aliases:
- /fi/go/using-associative-arrays/
date:                  2024-02-03T18:11:02.869709-07:00
model:                 gpt-4-0125-preview
simple_title:         "Assosiatiivisten taulukoiden käyttö"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/using-associative-arrays.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mitä & Miksi?

Assosiatiiviset taulukot, tunnetaan Go:ssa nimellä mapit, mahdollistavat avain-arvo parien tallentamisen, missä jokainen uniikki avain mappautuu arvoon. Ohjelmoijat käyttävät mappeja tehokkaaseen datan hakemiseen, muokkaamiseen ja elementtikokoelman ylläpitämiseen, joka voidaan nopeasti hakea käyttäen uniikkeja avaimia.

## Kuinka:

Mapin luominen ja alustaminen Go:ssa voidaan tehdä monilla eri tavoilla. Tässä on perusesimerkki, jolla pääset alkuun:

```go
package main

import "fmt"

func main() {
    // Mapin julistaminen ja alustaminen
    colors := map[string]string{
        "red":   "#FF0000",
        "green": "#00FF00",
        "blue":  "#0000FF",
    }

    fmt.Println(colors)
    // Tuloste: map[blue:#0000FF green:#00FF00 red:#FF0000]
}
```

Elementtien lisääminen tai päivittäminen tapahtuu näin:

```go
colors["white"] = "#FFFFFF"
fmt.Println(colors)
// Tuloste: map[blue:#0000FF green:#00FF00 red:#FF0000 white:#FFFFFF]
```

Arvon hakeminen avaimen perusteella on suoraviivaista:

```go
fmt.Println("Punaisen heksakoodi on:", colors["red"])
// Tuloste: Punaisen heksakoodi on: #FF0000
```

Elementin poistaminen tapahtuu `delete` funktion avulla:

```go
delete(colors, "red")
fmt.Println(colors)
// Tuloste: map[blue:#0000FF green:#00FF00 white:#FFFFFF]
```

Mapin ylitse iterointi tapahtuu for-silmukalla:

```go
for color, hex := range colors {
    fmt.Printf("Avain: %s Arvo: %s\n", color, hex)
}
```

Muista, Go:n mapit ovat järjestämättömiä. Iteraation järjestystä ei taata.

## Syväsukellus

Go:ssa mapit on toteutettu hajautustauluina. Jokainen merkintä mapissa koostuu kahdesta osasta: avaimesta ja arvosta. Avain hajautetaan tallentaakseen merkinnän, mikä mahdollistaa vakioaikaiset operaatiot pienelle datamäärälle ja keskimääräisen aikavaativuuden O(1) asianmukaisella hajautuksella, joka voi heikentyä O(n):ksi pahimmassa tapauksessa monien hajautusyhteentörmäysten kanssa.

Merkittävä huomio uusille Go-ohjelmoijille on, että map-tyypit ovat viitetyyppejä. Tämä tarkoittaa, kun passaat mapin funktioon, kaikki funktiossa tehdyt muutokset mapissa ovat näkyvissä kutsujalle. Tämä on erilaista esimerkiksi structin passaamisesta funktioon, missä struct kopioidaan, ellei sitä passata pointerin avulla.

Vaikka mapit ovat uskomattoman monipuolisia ja tehokkaita useimmissa käyttötapauksissa, jotka liittyvät assosiatiivisiin taulukoihin, suorituskykykritiikissä sovelluksissa saattaa olla hyödyllistä käyttää tietorakenteita, joilla on ennustettavampi suorituskyky, erityisesti jos avainjakaumat voivat aiheuttaa usein yhteentörmäyksiä.

Toinen vaihtoehto on harkita `sync.Map`:ia, joka on ollut saatavilla Go 1.9:stä lähtien, suunniteltu käyttötapauksiin, joissa avaimet kirjoitetaan vain kerran mutta luetaan monta kertaa, tarjoten tehokkuusparannuksia näissä skenaarioissa. Kuitenkin, tavanomaisissa Go-sovelluksissa, säännöllinen mapin käyttö on idiomaattista ja usein suositeltu lähestymistapa sen yksinkertaisuuden ja suoran tuen kielen puolesta.
