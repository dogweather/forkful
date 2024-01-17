---
title:                "Att hitta längden på en sträng"
html_title:           "Go: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng är ett vanligt problem som programmerare stöter på i sitt arbete. Det handlar helt enkelt om att räkna antalet tecken i en sträng, vilket kan vara användbart för att till exempel validera användarinput eller manipulera data i en applikation.

## Hur gör man:
Go-språket har inbyggda funktioner för att hitta längden på en sträng, som kan användas på enklast möjliga sätt:

```Go
str := "Hej, världen!"
strLen := len(str)
fmt.Println(strLen)
```

Detta kommer att skriva ut 13, eftersom strängen "Hej, världen!" består av 13 tecken. Det är viktigt att komma ihåg att den här funktionen räknar antalet bytes och inte antalet tecken, så om strängen innehåller specialtecken som t.ex. åäö kan längden vara annorlunda än förväntat.

För att fixa detta kan man använda paketet "unicode/utf8" och dess funktion "RuneCountInString" som returnerar antalet tecken i en sträng:

```Go
import "unicode/utf8"

str := "Hej, världen!"
strLen := utf8.RuneCountInString(str)
fmt.Println(strLen)
```

Detta kommer nu att skriva ut 12, vilket är det korrekta antalet tecken i strängen.

## Djupdykning:
Att hitta längden på en sträng är ett grundläggande problem som har funnits länge i programmeringsvärlden. I äldre språk som till exempel C var det inte lika enkelt att hitta längden på en sträng, och det krävde oftast en egen implementering av en funktion för detta ändamål.

Det finns också alternativ till Go-språkets inbyggda funktioner för att hitta längden på en sträng. Ett exempel är funktionen "Count" från paketet "strings", som låter dig räkna antalet gånger en viss del av en sträng förekommer:

```Go
import "strings"

str := "Hej, världen!"
subStr := "l"
count := strings.Count(str, subStr)
fmt.Println(count)
```

Detta kommer att skriva ut 2, eftersom tecknet "l" förekommer två gånger i strängen.

## Se även:
För mer information och användbara funktioner för hantering av strängar i Go-språket, se dokumentationen för paketet "strings": https://golang.org/pkg/strings/