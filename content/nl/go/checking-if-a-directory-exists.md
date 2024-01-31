---
title:                "Controleren of een directory bestaat"
date:                  2024-01-28T21:56:05.790848-07:00
model:                 gpt-4-0125-preview
simple_title:         "Controleren of een directory bestaat"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Controleren of een map bestaat, betekent bevestigen of een specifieke map aanwezig is op het bestandssysteem. Programmeurs doen dit om fouten te voorkomen, zoals proberen te lezen van of te schrijven naar een map die er niet is.

## Hoe:
De standaardbibliotheek van Go maakt het eenvoudig. Gebruik `os.Stat` en controleer op fouten met `os.IsNotExist`:

```go
package main

import (
	"fmt"
	"os"
)

func main() {
	dir := "/pad/naar/je/map"
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		fmt.Printf("Oeps: %v\n", err)
	} else {
		fmt.Println("Ja, het bestaat!")
	}
}
```

Voorbeeld van uitvoer als de map niet bestaat:

```
Oeps: stat /pad/naar/je/map: geen dergelijk bestand of map
```

En als het wel bestaat:

```
Ja, het bestaat!
```

## Diepgaand
Deze "bestaan-controle" is vanaf de vroege dagen onderdeel van Go, onderdeel van het robuuste `os`-pakket. Er is nog een andere manier: `ioutil.ReadDir` leest de map en retourneert een fout als deze niet bestaat. Maar waarom moeite doen? Het is minder efficiënt voor enkel het controleren van bestaan.

Onder de motorkap doet `os.Stat` een systeemaanroep om de bestands- of mapinformatie op te halen. Geen noodzaak om een aanroep voor elk bestand te doen wanneer één voldoende is.

In het verleden gebruikten programmeurs de tactiek om een bestand in de map aan te raken, maar dat is onnodige I/O. We willen efficiënte en elegante code. Go doet dit met eenvoud.

## Zie Ook
- Documentatie van Go's `os`-pakket: https://pkg.go.dev/os#Stat
- Bestandssysteemoperaties in Go: https://golang.org/pkg/io/ioutil/#ReadDir
- Meer over foutafhandeling in Go: https://blog.golang.org/error-handling-and-go
