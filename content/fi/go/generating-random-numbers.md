---
title:    "Go: Satunnaislukujen generointi"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi käyttää satunnaislukugeneraattoria Go-koodauksessa?

Satunnaislukujen generoiminen on usein välttämätöntä monissa ohjelmointitehtävissä, kuten pelien kehittämisessä tai tietokantojen täyttämisessä testausta varten. On tärkeää tietää, kuinka generoida satunnaislukuja oikein Go-kielellä, jotta voidaan varmistaa ohjelmiston luotettavuus ja suorituskyky.

## Kuinka tehdä se?

Go-kielellä satunnaislukujen generoiminen on helppoa. Alla on esimerkki koodista, joka generoi 5 satunnaislukua välillä 1-10 ja tulostaa ne näytölle:

```
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Aseta satunnaisgeneraattorin siemen millisekuntikellon nykyisen ajan mukaan
	rand.Seed(time.Now().UnixNano())

	// Generoi 5 satunnaislukua välillä 1-10 ja tulosta ne
	for i := 0; i < 5; i++ {
		fmt.Println(rand.Intn(10) + 1)
	}
}
```

Esimerkkitulos:

```
8
3
10
4
6
```

## Syvemmälle satunnaislukujen generointiin

Satunnaislukugeneraattorit käyttävät matemaattisia algoritmeja tuottamaan pseudo-satunnaisia numeroita. Nämä algoritmit perustuvat yleensä johonkin muuttujaan, kuten ajanhetkeen tai ohjelman suorituskertojen määrään, jotta jokainen generoitu satunnaisluku olisi erilainen. Tämä varmistaa, että luvut eivät toista samaa kaavaa ja näin ollen ovat mahdollisimman satunnaisia.

On tärkeää huomata, että satunnaislukugeneraattorit eivät varmasti tuota täysin satunnaisia lukuja. Lopputuloksena on aina jonkinlainen kaava tai toistumisen mahdollisuus. Tästä syystä satunnaislukujen käyttöön tietoturvan kannalta kriittisissä sovelluksissa tulisi käyttää erikoistuneempia työkaluja.

## Katso myös

- [Go:n virallinen satunnaislukugeneraattori-paketti](https://golang.org/pkg/math/rand/)
- [Satunnaislukujen käyttö tietoturvassa](https://www.schneier.com/academic/random.html)
- [Satunnaislukugeneraattorien vertailu ja arviointi](https://csrc.nist.gov/Projects/Random-Bit-Generation/Documentation-and-Software)
- [Avoimen lähdekoodin satunnaislukugeneraattori Go-kielelle](https://github.com/gofrs/uuid)