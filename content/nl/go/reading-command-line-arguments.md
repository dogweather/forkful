---
title:                "Commandoregelargumenten lezen"
date:                  2024-01-28T22:05:52.889433-07:00
model:                 gpt-4-0125-preview
simple_title:         "Commandoregelargumenten lezen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Commandoregelargumenten lezen stelt je programma in staat om invoer te nemen wanneer het vanuit een terminal wordt uitgevoerd, wat zijn gedrag kan sturen zonder waarden hard te coderen. Programmeurs gebruiken het om software-uitvoering aan te passen, gebruikersvoorkeuren te behandelen en te reageren op verschillende bedrijfsmodes.

## Hoe:

Go maakt het vrij eenvoudig om die commandoregelargumenten te grijpen met behulp van het `os`-pakket. Zo doe je dat:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args[1:] // os.Args[0] is het pad naar het programma zelf
	for i, arg := range args {
		fmt.Printf("Argument %d: %s\n", i+1, arg)
	}
}
```

Voer je programma zo uit:

```
$ go run jouwprogramma.go dit zijn commandoregel args
```

En je zou krijgen:

```
Argument 1: dit
Argument 2: zijn
Argument 3: commandoregel
Argument 4: args
```

Dat is het. Je hebt nu de macht om het gedrag van je programma vanuit de terminal te beïnvloeden.

## Diepere Duik

Lang voordat GUI's bestonden, waren commandoregelargumenten de standaard voor het vertellen aan programma's wat ze moesten doen. Ze stammen af van UNIX-conventies, die Go deels erft vanwege zijn relatie met POSIX-compatibele omgevingen.

Alternatieven voor het parseren van argumenten in Go omvatten het gebruik van meer geavanceerde pakketten zoals `flag` voor vlaggen (bijvoorbeeld, `--name=waarde`) of externe bibliotheken zoals `cobra` of `urfave/cli` voor het bouwen van complexe CLI-applicaties.

De `os.Args` array vangt alle argumenten op, met `os.Args[0]` zijnde het programma zelf. De eenvoud ervan is perfect voor eenvoudige taken, maar let op gevallen die gestructureerde commando's of vlaggen nodig hebben.

## Zie Ook

- Het `flag`-pakket voor een krachtiger optie-parsing: [https://pkg.go.dev/flag](https://pkg.go.dev/flag)
- Cobra voor het bouwen van krachtige commandoregeltoepassingen: [https://github.com/spf13/cobra](https://github.com/spf13/cobra)
- `urfave/cli` voor een eenvoudig, snel en leuk pakket voor het bouwen van CLIs in Go: [https://github.com/urfave/cli](https://github.com/urfave/cli)
