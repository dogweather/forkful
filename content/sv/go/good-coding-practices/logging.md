---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:54.991907-07:00
description: "Hur man g\xF6r: I Go kan loggning implementeras med hj\xE4lp av standardbibliotekets\
  \ paket `log`. Detta paket tillhandah\xE5ller enkla loggningsfunktioner, s\xE5som\u2026"
lastmod: '2024-03-13T22:44:37.399181-06:00'
model: gpt-4-0125-preview
summary: "I Go kan loggning implementeras med hj\xE4lp av standardbibliotekets paket\
  \ `log`."
title: Loggning
weight: 17
---

## Hur man gör:
I Go kan loggning implementeras med hjälp av standardbibliotekets paket `log`. Detta paket tillhandahåller enkla loggningsfunktioner, såsom att skriva till standardutmatningen eller till filer. Låt oss börja med ett grundläggande exempel på loggning till standardutmatningen:

```go
package main

import (
	"log"
)

func main() {
	log.Println("Detta är en grundläggande loggpost.")
}
```

Utmatning:
```
2009/11/10 23:00:00 Detta är en grundläggande loggpost.
```

Tidsstämpeln i början av loggposten läggs automatiskt till av paketet `log`. Nästa, låt oss utforska hur man loggar till en fil istället för standardutmatning:

```go
package main

import (
	"log"
	"os"
)

func main() {
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	log.SetOutput(file)
	log.Println("Denna loggpost går till en fil.")
}
```

Nu, låt oss implementera ett mer avancerat användningsfall: att anpassa loggformatet. Go tillåter dig att skapa en anpassad loggare med `log.New()`:

```go
package main

import (
	"log"
	"os"
)

func main() {
	logger := log.New(os.Stdout, "CUSTOM LOG: ", log.Ldate|log.Ltime|log.Lshortfile)
	logger.Println("Detta är ett anpassat loggmeddelande.")
}
```

Utmatning:
```
CUSTOM LOG: 2009/11/10 23:00:00 main.go:11: Detta är ett anpassat loggmeddelande.
```

Detta exempel prefixar varje loggmeddelande med "CUSTOM LOG: " och inkluderar datum, tid och källfilsplats.

## Djupdykning
Standardbibliotekets `log`-paket i Go är enkelt och tillräckligt för många applikationer, men det saknar några av de mer sofistikerade funktionerna som finns i tredjeparts loggningsbibliotek, såsom strukturerad loggning, loggrotation och loggning baserad på nivåer. Paket som `zap` och `logrus` erbjuder dessa avancerade funktioner och är väl ansedda inom Go-gemenskapen för deras prestanda och flexibilitet.

Strukturerad loggning, till exempel, gör det möjligt att logga data i ett strukturerat format (som JSON), vilket är särskilt användbart för moderna molnbaserade applikationer där loggar kan analyseras av olika verktyg eller tjänster. `zap`, i synnerhet, är känt för sin höga prestanda och låga allokeringsoverhead, vilket gör det lämpligt för applikationer där hastighet och effektivitet är avgörande.

Historiskt sett har loggning i Go utvecklats avsevärt sedan språkets början. Tidiga versioner av Go tillhandahöll de grundläggande loggningsfunktioner som vi ser i `log`-paketet. Dock, när språket växte i popularitet och komplexiteten i applikationer skrivna i Go ökade, började gemenskapen utveckla mer sofistikerade loggningsbibliotek för att möta sina behov. Idag, medan standard `log`-paketet fortfarande är ett genomförbart alternativ för enkla applikationer, vänder sig många utvecklare till dessa tredjepartslösningar för mer komplexa loggningskrav.
