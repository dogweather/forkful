---
title:    "Go: Kontroll av existensen för en mapp"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Varför

I Go-programmering, är det viktigt att kunna kontrollera om en mapp existerar i ditt system. Detta kan vara användbart när du skriver program som behöver läsa eller skriva filer på en specifik plats.

## Så här gör du

För att kontrollera om en mapp finns i ditt system, kan du använda `os.Stat` funktionen i Go. Detta låter dig hämta filinformation för en given sökväg. Om sökvägen är en mapp, kommer `os.Stat` funktionen att returnera ett `os.FileInfo` objekt. Annars, om sökvägen inte är en mapp, kommer det att returnera ett fel.

Här är ett exempel på hur du kan använda `os.Stat` för att kontrollera om en mapp existerar:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	path := "mappen/som/du/vill/kontrollera"
	
	_, err := os.Stat(path)
	
	if err != nil {
		fmt.Println("Mappen finns inte!")
	} else {
		fmt.Println("Mappen finns!")
	}
}
```

Om mappen finns, kommer outputen att vara "Mappen finns!". Annars, kommer outputen att vara "Mappen finns inte!".

## Djupdykning

När du använder `os.Stat` för att kontrollera om en mapp existerar, är det viktigt att notera att funktionen bara kontrollerar om den angivna sökvägen existerar. Det betyder att om en fil med samma namn som mappen finns i samma plats, kommer `os.Stat` att returnera felmeddelandet "permission denied" istället för att indikera att mappen inte finns.

En annan sak att tänka på är att även om `os.Stat` låter dig kontrollera om en mapp existerar, kommer det inte automatiskt att skapa mappen om den inte finns. Detta är något du måste göra manuellt med hjälp av `os.Mkdir` funktionen.

## Se även

- [os.Stat dokumentation](https://golang.org/pkg/os/#Stat)
- [os.Mkdir dokumentation](https://golang.org/pkg/os/#Mkdir)