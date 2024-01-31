---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:56:28.851997-07:00
simple_title:         "Kontrollera om en katalog finns"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp finns är att se om en specifik mapp existerar på filsystemet. Programmerare gör det för att undvika fel när de försöker läsa från eller skriva till en mapp som inte finns.

## Hur gör man:
I Go använder du paketet `os` för att kontrollera om en mapp finns. Om mappen saknas kan `os.Stat()` kasta ett fel som du kan granska.

```go
package main

import (
	"fmt"
	"os"
)

func main() {
	mappNamn := "./enTestMapp"

	if _, err := os.Stat(mappNamn); os.IsNotExist(err) {
		fmt.Printf("Mappen '%s' finns inte.\n", mappNamn)
	} else {
		fmt.Printf("Mappen '%s' finns.\n", mappNamn)
	}
}
```

Körs programmet och `enTestMapp` inte finns, blir utskriften:
```
Mappen 'enTestMapp' finns inte.
```

Annars, om mappen finns:
```
Mappen 'enTestMapp' finns.
```

## Djupdykning
Historiskt sett har filsystemshanteringen alltid varit en viktig del av programmering. Före `os`-paketet i Go, använde många andra språk liknande funktioner som `stat()` i C eller `File.Exists()` i .NET. I Go förlitar vi oss på `os.Stat()` för att få information om filstatus. Ett alternativ är att försöka öppna mappen med `os.Open()`, vilket också kan indikera om mappen finns eller ej beroende på om ett fel uppstår. Med dessa implementationer är det viktigt att hantera potentiella race conditions där mappens status kan ändras mellan att du kontrollerar existensen och din nästa operation.

## Se även
- Go dokumentation om `os`-paketet: https://pkg.go.dev/os
- Go blogg om filsystemshanteringen: https://blog.golang.org/io2015
- Artikel om race conditions: https://en.wikipedia.org/wiki/Race_condition
