---
title:                "Uuden projektin aloittaminen"
html_title:           "Go: Uuden projektin aloittaminen"
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

## Kuinka aloittaa uusi projekti Go:n avulla

Go on modernein ja tehokkain ohjelmointikieli, joten miksi ei aloittaisi uutta projekti sen avulla? Se on myös erittäin helppo oppia ja käyttää, joten aloittaminen onnistuu vaivatta.

```Go
package main
import "fmt"
func main() {
    fmt.Println("Tervetuloa uuteen projektiin Go:n avulla!")
}
```

Tämä yksinkertainen koodinpätkä esittelee Go:n perusmallin, jossa tuodaan "fmt" paketti, joka mahdollistaa tekstin tulostamisen konsoliin ja käytetään sitten "fmt.Println()" funktiota tekstin tulostamiseen. Voit muokata koodia ja nähdä, miten se vaikuttaa outputiin. 

```Go
package main
import "fmt"
func main() {
    fmt.Println("Uusi projekti on matkalla!")
}
```

Output:
```
Uusi projekti on matkalla!
```

## Syvemmälle

Aloittaessasi uuden projektin Go:lla, on tärkeää pitää mielessä muutamia avainasioita. Ensinnäkin, Go:n virallinen kotisivu tarjoaa erittäin hyödyllisen oppaan aloittelijoille, jossa käydään läpi kielen perusominaisuudet ja esimerkkikoodia. Toiseksi, Go:n yhteisö on valtavan suuri ja tarjoaa paljon tukea ja apua, joten älä epäröi etsiä vastauksia kysymyksiisi foorumeilta tai Slack-yhteisöistä.

Lisäksi, Go:n voimakkaan standardikirjaston ansiosta on suositeltavaa tutustua siihen, jotta pystyt hyödyntämään kaikkia sen tarjoamia mahdollisuuksia projektissasi. Lopuksi, käytä aikaa ja tutustu Go:n parhaisiin käytäntöihin ja suosituksiin, jotta pystyt kirjoittamaan tehokasta ja hyvin dokumentoitua koodia.

## Katso myös

- [Go:n virallinen dokumentaatio](https://golang.org/doc/)
- [Go:n yhteisöfoorumi](https://github.com/golang/go/wiki/Forums)
- [Go:n parhaat käytännöt](https://github.com/golang/go/wiki/CodeReviewComments)