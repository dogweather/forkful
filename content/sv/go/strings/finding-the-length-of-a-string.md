---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:59.179943-07:00
description: "Att hitta l\xE4ngden p\xE5 en str\xE4ng i Go handlar om att best\xE4\
  mma antalet tecken den inneh\xE5ller. Programmerare utf\xF6r rutinm\xE4ssigt denna\
  \ operation f\xF6r att\u2026"
lastmod: '2024-02-25T18:49:35.717359-07:00'
model: gpt-4-0125-preview
summary: "Att hitta l\xE4ngden p\xE5 en str\xE4ng i Go handlar om att best\xE4mma\
  \ antalet tecken den inneh\xE5ller. Programmerare utf\xF6r rutinm\xE4ssigt denna\
  \ operation f\xF6r att\u2026"
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng i Go handlar om att bestämma antalet tecken den innehåller. Programmerare utför rutinmässigt denna operation för att effektivt manipulera strängar, oavsett om det är för validering, extraktion av delsträngar eller helt enkelt för att genomdriva begränsningar i användarinmatningar.

## Hur man gör:
I Go behandlas strängar som omutbara sekvenser av bytes. Du kan hitta längden på en sträng med hjälp av den inbyggda `len()`-funktionen som returnerar antalet bytes, inte nödvändigtvis antalet tecken. Så här använder du den:

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// Använder len() för att hitta bytelängden
	str := "Hello, 世界"
	byteLength := len(str)
	fmt.Println("Byte Längd:", byteLength) // Utmatning: Byte Längd: 13

	// För att exakt få antalet tecken eller runor i en sträng
	runeLength := utf8.RuneCountInString(str)
	fmt.Println("Run Längd:", runeLength) // Utmatning: Run Längd: 9
}
```
Den första metoden som använder `len()` kanske inte alltid ger det förväntade resultatet eftersom den räknar bytes. För strängar som innehåller icke-ASCII-tecken (som "世界"), bör `RuneCountInString` från `unicode/utf8`-paketet användas istället för att räkna Unicode kodpunkter korrekt.

## Fördjupning
Innan Go 1 fanns ingen strikt avgränsning för hantering av strängar som sekvenser av bytes kontra sekvenser av tecken. Efter Go 1, med antagandet av UTF-8 som standardkodningsschema för strängar, krävdes tydligare tillvägagångssätt. Funktionen `len()` fungerar perfekt för ASCII-strängar, där tecken representeras i ett enda byte. Dock, då Go-applikationer blev mer globala, och behovet av att stödja en mängd olika språk och teckenuppsättningar växte, visade den förenklade metoden med `len()` begränsningar.

Introduktionen och användningen av `utf8.RuneCountInString()` svarar på dessa begränsningar genom att ge ett sätt att räkna faktiska Unicode-tecken (runor i Go-terminologi). Denna metod säkerställer att längdberäkningen är oberoende av kodningsspecifikheterna för UTF-8, där tecken kan sträcka sig över flera bytes.

Ett alternativt tillvägagångssätt för att traversera och manipulera strängar, mer i linje med Gos samtidighet och effektivitetsetos, kan innebära att behandla strängar som skivor av runor. Dock kräver denna metod ett konverteringssteg och löser inte omedelbart alla intrikatesser med Unicode (t.ex. kombinerade tecken).

Sammanfattningsvis, medan `len()` är lämplig för bytelängd och är effektiv för ASCII-text, är `utf8.RuneCountInString()` ett pålitligare val för en globalt kompatibel applikation. Ändå uppmuntras utvecklare att förstå avvägningarna i prestanda och minnesanvändning som dessa val medför.
