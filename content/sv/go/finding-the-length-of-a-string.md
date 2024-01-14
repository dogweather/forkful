---
title:                "Go: Att hitta längden på en sträng"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden på en sträng är en grundläggande kunskap inom Go-programmering. Det kan hjälpa dig att förstå hur data hanteras och manipuleras i program. Dessutom är det ett användbart verktyg för många olika problem.

## Hur man gör

Det finns flera olika sätt att hitta längden på en sträng i Go. En metod är att använda den inbyggda funktionen len (), som returnerar antalet tecken i en sträng. Här är ett exempel:

```Go
str := "Hej! Hur mår du?"
fmt.Println(len(str))
```

Output: 17

En annan metod är att använda en loop för att räkna varje tecken i strängen och öka en räknare. Här är ett exempel på hur man kan göra det:

```Go
str := "Go är ett fantastiskt programmeringsspråk!"
var length int = 0
for range str {
	length++
}
fmt.Println(length)
```

Output: 37

## Djupdykning

Att förstå hur funktionen len () fungerar bakom kulisserna kan hjälpa dig att förbättra din kodning. När du använder len () för en sträng, returneras antalet byte som är nödvändigt för att lagra strängen i minnet. Det beror på att Go använder UTF-8 för att representera strängar, vilket kan leda till att antalet byte inte är samma som antalet tecken.

En annan viktig aspekt att tänka på när du arbetar med strängar är skillnaden mellan bytes och runor. En byte representerar 8 bitar medan en runa representerar en enskild Unicode-tecken. Detta kan påverka längden på en sträng baserat på språket som används.

## Se även

- [Officiell Go-dokumentation: Längd och kapacitet](https://golang.org/ref/spec#Length_and_capacity)
- [Officiell Go-dokumentation: Strängar](https://golang.org/ref/spec#Runes_and_string_literals)
- [Tutorial: Go strängar](https://ourcodeworld.com/articles/read/189/top-10-best-practices-to-work-with-strings-in-golang)
- [Exempel: Go strängmanipulering](https://gobyexample.com/string-functions)