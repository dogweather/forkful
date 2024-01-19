---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/printing-debug-output.md"
---

{{< edit_this_page >}}

# Go (Golang) Ohjelmoinnin Debug Tulostus 

## Mikä & Miksi?

"Debug-tulostus" on tiedon lähettämistä konsoliin virheenkorjausta (debugging) varten. Ohjelmoijat käyttävät sitä selvittääkseen ohjelmakoodissaan olevat virheet.

## Miten Näin:

Simple esimerkki Go-koodista, joka tulostaa debug-informaatiota.

```Go
package main

import (
	"fmt"
	"runtime"
	"path/filepath"
)

func Log(format string, a ...interface{}) {
	_, file, line, _ := runtime.Caller(1)
	fmt.Printf("%s:%d: ", filepath.Base(file), line)
	fmt.Printf(format, a...)
	fmt.Println()
}

func main() {
	Log("Hello, %s", "world")
}
```

Tämä tuottaa tulosteen, kuten:

```
main.go:17: Hello, world
```

## Syvällisempi Sukellus:

Debug-tulostuksella on pitkä historia, ja se on yksi vanhimmista virheenkorjausmenetelmistä. Go-kielessä on useita tapoja tehdä debug-tulostus, kuten esim. `fmt.Print` ja `log.Print` funktiot.

On muitakin mahdollisuuksia tuottaa debug-tulostus, esimerkiksi `logrus` tai `zap` -kirjastot. Ne tarjoavat monipuolisemmat ominaisuudet, kuten lokitason hallinnan ja rakenteelliset logit.

Käyttämämme `Log`-funktio käyttää `runtime.Caller`-funktiota saamaan tiedon siitä, missä ja milloin funktio kutsuttiin. Tämä on erittäin hyödyllistä, kun haluat tietää, mistä koodin osasta viesti tuli.

## Katso Myös:

1. Go virallinen dokumentaatio: [fmt](https://golang.org/pkg/fmt/), [log](https://golang.org/pkg/log/)
2. Debug-tulostus kirjastot: [logrus](https://github.com/sirupsen/logrus), [zap](https://github.com/uber-go/zap)
3. Runtime paketti: [runtime](https://golang.org/pkg/runtime/)