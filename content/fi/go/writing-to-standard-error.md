---
title:                "Kirjoittaminen vakiovirheeseen"
date:                  2024-01-19
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
"## Mitä & Miksi?"

Kirjoittaminen vakiovirheeseen (stderr) tarkoittaa virheiden ja lokiviestien tulostamista erilliseen virtaan. Sitä käytetään, koska se auttaa erottamaan ohjelman pääasiallisen ulostulon ja virhetiedot toisistaan, mikä tekee virheenjäljityksestä ja lokien analysoinnista helpompaa.

## How to:
"## Kuinka tehdä:"

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Tavanomainen ulostulo
	fmt.Println("Hei, stdout!")

	// Virheviesti stderr:ään
	_, err := fmt.Fprintln(os.Stderr, "Virhe: jotakin meni pieleen.")
	if err != nil {
		panic(err)
	}
}
```

Sample output:
```
Hei, stdout!
Virhe: jotakin meni pieleen.
```

## Deep Dive
"## Syväsukellus"

Alun perin UNIX-järjestelmissä kehitetty virturointi (streaming) mahdollisti tiedon eriyttämisen kolmeen virtaan: stdin, stdout ja stderr. Stderr:ää käytetään virheiden raportointiin, mikä sallii erillisen käsittelyn, kuten ohjaamisen tiedostoon tai putkituksen toiseen prosessiin. Go:ssa `os`-paketin kautta kirjoittaminen stderr-virtaan on suoraviivaista. Vaihtoehtoisesti `log`-pakettia voidaan käyttää lokiviestien käsittelyssä, jolloin stderr käytetään oletuksena lokien kirjoitusvirtana.

## See Also
"## Katso myös"

- Go:n dokumentaatio virheidenkäsittelyyn: https://golang.org/pkg/errors/
- `log`-paketin dokumentaatio: https://golang.org/pkg/log/
- UNIX-standardivirroista: https://en.wikipedia.org/wiki/Standard_streams
