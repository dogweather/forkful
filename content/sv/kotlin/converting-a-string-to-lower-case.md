---
title:    "Kotlin: Omvandla en sträng till gemener"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Att kunna konvertera en sträng till gemener är en nyttig och användbar kunskap inom Kotlin programmering. Detta låter dig behandla och manipulera text på ett mer flexibelt sätt och gör det enklare att jämföra och sortera strängar.

## Hur man gör det
För att konvertera en sträng till gemener i Kotlin, kan du använda stringens inbyggda funktion `toLowerCase()`. Detta tar bort eventuella stora bokstäver i strängen och ersätter dem med motsvarande gemener.

```Kotlin
val str = "HEJ ALLA KOTLIN-PROGRAMMERARE"
println(str.toLowerCase())
```
Output:
`hej alla kotlin-programmerare`

Det är viktigt att notera att funktionen `toLowerCase()` returnerar en ny sträng och påverkar inte den ursprungliga strängen. Om du vill ändra den ursprungliga strängen permanent, måste du tilldela det nya värdet till den variabel som innehåller strängen.

```Kotlin
var str = "HEJ ALLA KOTLIN-PROGRAMMERARE"
str = str.toLowerCase()
println(str)
```

Output:
`hej alla kotlin-programmerare`

## Djupdykning
Konverteringen från stora bokstäver till gemener beror på vilket språk som används i din miljö. Standardbiblioteket i Kotlin använder sig av Unicode-standard som anger hur stora och små bokstäver i ett språk förhåller sig till varandra. Detta betyder att konverteringen kan variera beroende på vilket språk som är inställt i ditt system.

Det är också viktigt att notera att konverteringen till gemener inte bara gäller för bokstäver utan även för andra tecken som till exempel accenter och skiljetecken. I vissa fall kan detta leda till att tecken försvinner eller förändras, därför är det alltid bäst att testa konverteringen på din specifika sträng för att se till att resultatet blir det önskade.

## Se även
- [Kotlin Strings](https://kotlinlang.org/docs/strings.html)
- [Unicode Standard](https://www.unicode.org/standard/index.html)