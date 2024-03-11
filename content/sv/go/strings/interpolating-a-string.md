---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:36.667599-07:00
description: "Str\xE4nginterpolering \xE4r en metod f\xF6r att konstruera str\xE4\
  ngar som inkorporerar variabler, vilket m\xF6jligg\xF6r dynamisk skapande av str\xE4\
  ngar. Programmerare g\xF6r\u2026"
lastmod: '2024-03-11T00:14:10.680737-06:00'
model: gpt-4-0125-preview
summary: "Str\xE4nginterpolering \xE4r en metod f\xF6r att konstruera str\xE4ngar\
  \ som inkorporerar variabler, vilket m\xF6jligg\xF6r dynamisk skapande av str\xE4\
  ngar. Programmerare g\xF6r\u2026"
title: "Interpolera en str\xE4ng"
---

{{< edit_this_page >}}

## Vad och varför?

Stränginterpolering är en metod för att konstruera strängar som inkorporerar variabler, vilket möjliggör dynamisk skapande av strängar. Programmerare gör detta för att anpassa meddelanden, konstruera URL:er, skapa SQL-frågor och mer, vilket möjliggör mer läsbar och underhållbar kod.

## Hur man gör:

I Go uppnås stränginterpolering vanligtvis med hjälp av `fmt`-paketet, särskilt med funktionen `Sprintf`, som låter dig injicera variabler i en sträng genom att specificera formatteringsverb. Verben är platshållare i formatsträngen och ersätts av de givna variablernas värden. Så här använder du det:

```go
package main

import (
    "fmt"
)

func main() {
    name := "Jane"
    age := 28

    // Använder Sprintf för stränginterpolering
    message := fmt.Sprintf("Hej, mitt namn är %s och jag är %d år gammal.", name, age)
    fmt.Println(message) // Utmatning: Hej, mitt namn är Jane och jag är 28 år gammal.
}
```

Observera att `%s` används för strängar och `%d` för heltal. `fmt`-paketdokumentationen ger en omfattande lista med formatteringsverb för olika datatyper.

## Djupdykning

Konceptet med stränginterpolering finns i många programmeringsspråk, även om det skiljer sig åt i syntax och kapaciteter. I Go, även om `fmt`-pakets funktion `Sprintf` är det mest använda tillvägagångssättet, kanske det inte alltid är det mest effektiva, speciellt för enkla sammanfogningar eller när man arbetar inom högpresterande kod. 

`fmt`-paketet använder reflektion för att dynamiskt tolka variablernas typer vid körning, vilket, även om det är flexibelt, medför overhead. I scenarier där prestanda är avgörande, kan direkt strängsammanfogning eller typen `strings.Builder` erbjuda bättre alternativ. Direkt sammanfogning är rakt på sak men kan bli otymplig med flera variabler. `strings.Builder`, å andra sidan, ger ett mer prestandaeffektivt och läsbart sätt att bygga komplexa strängar i en loop eller när man hanterar många variabler:

```go
var sb strings.Builder
sb.WriteString("Hej, mitt namn är ")
sb.WriteString(name)
sb.WriteString(" och jag är ")
sb.WriteString(strconv.Itoa(age))
sb.WriteString(" år gammal.")
message := sb.String()

fmt.Println(message) // Ger samma utmatning som tidigare
```

I slutändan beror valet mellan `fmt.Sprintf`, direkt sammanfogning och `strings.Builder` på de specifika kraven i din applikation, så som komplexiteten hos strängen som konstrueras och prestandahänsyn.
