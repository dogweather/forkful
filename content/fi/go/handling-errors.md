---
title:                "Virheiden käsittely"
date:                  2024-01-26T00:53:26.362143-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheiden käsittely"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/handling-errors.md"
---

{{< edit_this_page >}}

## Mikä ja Miksi?

Virheiden käsittely Go:ssa tarkoittaa ohjelman aikana tapahtuvien virhetilanteiden tyylikästä havaitsemista ja niihin reagoimista. Sen avulla estetään ohjelman kaatumiset ja varmistetaan, että ohjelmamme toimivat ennustettavasti, vaikka asiat menisivätkin vinoon.

## Kuinka:

Go käyttää eksplisiittistä virheenkäsittelyä. Tämä tarkoittaa, että tarkistat virheen palautumisen joka kerta kun kutsut funktiota. Ei poikkeuksia. Tältä se näyttää:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("Voi ei:", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// Tehdään niin kuin jotain menisi pieleen
	return fmt.Errorf("jokin meni pieleen")
}
```

Aja tämä, ja saat tulokseksi:

```
Voi ei: jokin meni pieleen
```

Entä jos se onnistuu?

```Go
func doSomething() error {
	// Kaikki hyvin tällä kertaa
	return nil
}
```

Ei tulostusta. Siistiä, ei uutisia on hyviä uutisia.

## Syväsukellus:

Go:ssa virheenkäsittely on ollut kiistelyn aiheena. Alusta asti Go päätti olla käyttämättä poikkeuksia, ja siten valitsi eksplisiittisemmän lähestymistavan, jota jotkut kehittäjät rakastavat sen yksinkertaisuuden vuoksi ja toiset pitävät verbosisena. Sisäänrakennettu `error`-tyyppi on rajapinta. Mikä tahansa tyyppi, jolla on `Error() string` -metodi, täyttää sen. Tämä liittyy Go:n filosofiaan, joka korostaa yksinkertaisuutta ja eksplisiittisyyttä.

Vaihtoehtoja? On `panic` ja `recover` -kaksikko, mutta ne ovat poikkeuksellisille tapauksille (sanaleikki tarkoitettu) kun ohjelma ei voi jatkua. Ajattele `panic`-toimintoa kuin hätäpoistumisnappia, jota painat kun tiedät, ettei paluuta ole. Käytä sitä säästeliäästi.

Mitä tulee valtavirran virheenkäsittelyyn, Go 1.13 toi mukanaan virheiden käärimisen, mikä helpottaa "virheketjun" selvittämistä funktioilla kuten `errors.Is()` ja `errors.As()`.

## Katso Myös:

Kaikkea Go:n virheenkäsittelystä:

- Go Blogi virheenkäsittelystä: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Tehokas Go – Virheenkäsittelyn osio: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Go 1.13 Virheenkäärimisen dokumentaatio: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Dave Cheney'n postaus virheenkäsittelystrategioista: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)
