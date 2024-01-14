---
title:                "Go: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en grundläggande uppgift inom programmering och är användbar för att lösa olika problem, som att validera inmatningar eller jämföra strängar.

## Så här gör man

För att hitta längden på en sträng i Go kan man använda sig av den inbyggda funktionen `len()` tillsammans med strängvariabelns namn. Detta kommer att returnera antalet tecken i strängen.

```Go
str := "Hej världen!"
fmt.Println(len(str))

// Output: 13
```

För att hantera specialtecken eller Unicode-tecken i strängen, kan man använda funktionen `utf8.RuneCountInString()` som kommer att returnera antalet tecken enligt Unicode-standard istället för antalet bytes.

```Go
str := "Hej världen!"
fmt.Println(utf8.RuneCountInString(str))

// Output: 12
```

## Djupdykning

När man använder funktionen `len()` så räknas även det nollte tecknet, vilket är ett tomt tecken, med. Detta kan orsaka förvirring om man inte är medveten om det. Om man behöver ta bort detta tomma tecken kan man göra det genom att använda funktionen `TrimSpace()` från paketet `strings`.

Det är även möjligt att hitta längden på en array av bytes genom att använda funktionen `len()` på denna array, vilket returnerar antalet element istället för antalet bytes.

## Se även

- [Go strings tutorial](https://www.programiz.com/go-programming/strings)
- [Package strings](https://golang.org/pkg/strings/)
- [Package utf8](https://golang.org/pkg/utf8/#RuneCountInString)