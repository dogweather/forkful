---
title:                "Schrijven naar standaardfout"
date:                  2024-01-28T22:13:31.827803-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schrijven naar standaardfout"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Schrijven naar de standaardfout (stderr) is hoe je programma fouten en waarschuwingen rapporteert. Programmeurs doen dit om reguliere output (stdout) te scheiden van foutmeldingen, wat het makkelijker maakt om problemen te beheren en te traceren.

## Hoe te:

In Go schrijf je naar de standaardfout met behulp van de `os.Stderr` bestandsdescriptor van het `os` pakket. Hier is hoe je dat doet:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	bericht := "Fout: er is iets misgegaan!"
	_, err := fmt.Fprintln(os.Stderr, bericht)

	if err != nil {
		panic(err)
	}
}
```

Voorbeelduitvoer naar stderr ziet er mogelijk zo uit:

```
Fout: er is iets misgegaan!
```

## Diepgaande duik

Historisch gezien bieden Unix-achtige besturingssystemen drie standaardstromen: stdin, stdout en stderr. Go erft dit concept. Alternatieven omvatten logboekpakketten zoals `log` of `zap`, die meer controle bieden over uitvoerformaat en bestemming. Bij direct schrijven naar stderr gebruikt Go `os.Stderr`, wat `io.Writer` implementeert, waardoor het consistent is met Go's algemene benadering van I/O door een goed gedefinieerde interface te bieden.

## Zie ook

- De Go Blog over foutafhandeling: https://blog.golang.org/error-handling-and-go
- Documentatie van het `log` pakket: https://golang.org/pkg/log/
- `zap` logger: https://godoc.org/go.uber.org/zap
