---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:37.354543-07:00
description: "Satunnaislukujen generointi ohjelmoinnissa on kyse sellaisten lukusarjojen\
  \ luomisesta, joita ei voida kohtuullisesti ennustaa paremmin kuin sattumalta.\u2026"
lastmod: '2024-02-25T18:49:53.024817-07:00'
model: gpt-4-0125-preview
summary: "Satunnaislukujen generointi ohjelmoinnissa on kyse sellaisten lukusarjojen\
  \ luomisesta, joita ei voida kohtuullisesti ennustaa paremmin kuin sattumalta.\u2026"
title: Satunnaisten numeroiden generointi
---

{{< edit_this_page >}}

## Mitä & Miksi?

Satunnaislukujen generointi ohjelmoinnissa on kyse sellaisten lukusarjojen luomisesta, joita ei voida kohtuullisesti ennustaa paremmin kuin sattumalta. Ohjelmoijat tekevät niin monista syistä, mukaan lukien simulaatiot, pelit ja turvallisuussovellukset, joissa arvaamattomuus on avain toiminnallisuuteen tai salassapitoon.

## Kuinka:

Go:ssa satunnaislukuja generoidaan käyttämällä `math/rand` -pakettia pseudo-satunnaislukuja varten tai `crypto/rand` -pakettia kryptografisesti turvallisia pseudo-satunnaislukuja varten. Tutkitaan molempia.

### Käyttäen `math/rand` Pseudo-satunnaislukuja Varten

Ensimmäiseksi, tuo `math/rand` -paketti ja `time` -paketti alustamaan generaattoria. Alustaminen varmistaa, että saat eri lukusarjan joka suorituskerralla.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("Satunnainen luku:", rand.Intn(100)) // Generoi luvun väliltä 0 ja 99
}
```

Esimerkkituloste: `Satunnainen luku: 42`

### Käyttäen `crypto/rand` Kryptografisesti Turvallisia Pseudo-satunnaislukuja Varten

Turvallisuusherkkien sovellusten osalta `crypto/rand` -paketti sopii, sillä se generoi satunnaislukuja, jotka ovat vaikeasti ennustettavissa, tehden niistä sopivia kryptografisiin toimenpiteisiin.

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("Turvallinen satunnainen luku:", n)
}
```

Esimerkkituloste: `Turvallinen satunnainen luku: 81`

## Syväsukellus

Perusero `math/rand` ja `crypto/rand` -pakettien välillä Go:ssa juontaa juurensa niiden entropian lähteestä ja niiden käyttötarkoituksista. `math/rand` generaattori tuottaa pseudo-satunnaislukuja perustuen alkusijoitukseen; näin ollen sekvenssi on deterministinen ja voidaan ennustaa, jos siemen on tiedossa. Tämä soveltuu skenaarioihin, joissa suorituskyky ja ei absoluuttinen arvaamattomuus on avainasia, kuten simulaatioissa tai peleissä.

Toisaalta, `crypto/rand` johtaa satunnaisuuden käyttöjärjestelmän alta, tehden siitä sopivan kryptografisiin käyttöihin, joissa arvaamattomuus on kriittistä. Tämä kuitenkin tulee suorituskyvyn ja monimutkaisuuden kustannuksella numeroiden käsittelyssä (kuten käsiteltäessä `*big.Int` -tyyppiä kokonaisluvuille).

Historiallisesti tietokoneiden satunnaislukujen generoinnin käsite on aina liikkunut todellisen "satunnaisuuden" rajamailla, varhaisjärjestelmien nojatessa voimakkaasti deterministisiin algoritmeihin, jotka matkivat satunnaisuutta. Tietokoneiden kehittyessä, myös nämä algoritmit kehittyivät, sisällyttäen yhä monimutkaisempia entropian lähteitä ympäristöstään.

Huolimatta näistä edistysaskeleista, täydellisen satunnaisuuden tavoittelu tietokoneissa on perustavanlaatuisesti paradoksaalista, ottaen huomioon tietokoneiden itsensä deterministisen luonteen. Tämä on miksi, useimmissa sovelluksissa, joissa ennakoitavuus olisi haitallista, kryptografisesti turvalliset pseudo-satunnaisluvut lähteistä kuten `crypto/rand` ovat parempi vaihtoehto, huolimatta niiden ylikuormituksesta.

Ytimessään, Gon lähestymistapa kahdella erillisellä paketilla satunnaislukujen generointiin tyylikkäästi käsittelee kompromisseja suorituskyvyn ja turvallisuuden välillä, antaen kehittäjien valita perustuen heidän spesifeihin tarpeisiinsa.
