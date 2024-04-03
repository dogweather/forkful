---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:29.555727-07:00
description: "Att ta bort citattecken fr\xE5n en str\xE4ng i Go handlar om att eliminera\
  \ de inledande och avslutande citattecknen (`\"` eller `'`) fr\xE5n en given str\xE4\
  ng.\u2026"
lastmod: '2024-03-13T22:44:37.379074-06:00'
model: gpt-4-0125-preview
summary: "Att ta bort citattecken fr\xE5n en str\xE4ng i Go handlar om att eliminera\
  \ de inledande och avslutande citattecknen (`\"` eller `'`) fr\xE5n en given str\xE4\
  ng."
title: "Ta bort citattecken fr\xE5n en str\xE4ng"
weight: 9
---

## Vad & Varför?

Att ta bort citattecken från en sträng i Go handlar om att eliminera de inledande och avslutande citattecknen (`"` eller `'`) från en given sträng. Programmerare behöver ofta utföra denna uppgift för att sanera användarinput, tolka textdata mer effektivt eller förbereda strängar för vidare bearbetning som kräver innehåll utan citattecken.

## Hur:

Go erbjuder flera metoder för att ta bort citattecken från en sträng, men en av de mest raka vägarna är att använda funktionerna `Trim` och `TrimFunc` som tillhandahålls av paketet `strings`. Så här gör du:

```go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	quotedString := `"This is a 'quoted' string"`

	// Använder strings.Trim för att ta bort specifika citattecken
	unquoted := strings.Trim(quotedString, `"'`)
	fmt.Println("Använder strings.Trim:", unquoted)

	// Anpassad metod som använder strings.TrimFunc för mer kontroll
	unquotedFunc := strings.TrimFunc(quotedString, func(r rune) bool {
		return r == '"' || r == '\''
	})
	fmt.Println("Använder strings.TrimFunc:", unquotedFunc)
}
```

Detta exempel visar två metoder för att ta bort både dubbla (`"`) och enkla (`'`) citattecken. Funktionen `strings.Trim` är enklare och fungerar bra när du vet exakt vilka tecken som ska tas bort. Å andra sidan ger `strings.TrimFunc` mer flexibilitet, vilket tillåter dig att specificera en anpassad funktion för att avgöra vilka tecken som blir borttagna. Exemplet på utmatning från ovanstående kod är:

```
Använder strings.Trim: This is a 'quoted' string
Använder strings.TrimFunc: This is a 'quoted' string
```

Båda metoderna tar effektivt bort de inledande och avslutande citattecknen från strängen.

## Fördjupning

Funktionerna `Trim` och `TrimFunc` från paketet `strings` är del av Gos omfattande standardbibliotek, designade för att erbjuda kraftfulla, men raka möjligheter för strängmanipulation utan behovet av tredjepartspaket. Historiskt sett kommer behovet av att hantera och manipulera strängar effektivt från Gos huvudfokus på nätverksservrar och dataparsers, där strängbearbetning är en vanlig uppgift.

Ett anmärkningsvärt aspekt av dessa funktioner är deras implementering baserad på runor (Gos representation av en Unicode-kodpunkt). Denna design möjliggör att de smidigt kan hantera strängar som innehåller flerbytestecken, vilket gör Gos tillvägagångssätt till strängmanipulation både robust och Unicode-vänligt.

Även om direktanvändning av `Trim` och `TrimFunc` för att ta bort citattecken är bekvämt och idiomatiskt i Go, är det värt att nämna att för mer komplex strängprocessering (t.ex. nästlade citattecken, escapeade citattecken) kan reguljära uttryck (via paketet `regexp`) eller manuell parsing erbjuda bättre lösningar. Dock kommer dessa alternativ med ökad komplexitet och prestandaöverväganden. Därför, för enkel borttagning av citattecken, utgör de demonstrerade metoderna en bra balans mellan enkelhet, prestanda och funktionalitet.
