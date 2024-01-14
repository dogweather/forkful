---
title:    "Go: Att hitta längden på en sträng"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden på en sträng är en grundläggande färdighet inom Go-programmering. Genom att kunna göra detta kan du hantera data mer effektivt och korrekt i dina program. Det är en enkel uppgift som kan ge stor nytta i dina projekt.

## Hur man gör det

Att hitta längden på en sträng i Go är väldigt enkelt. Du kan använda inbyggda funktionen ```len``` för att få längden på en sträng. Låt oss ta en titt på ett exempel:

```
func main() {
	str := "Detta är en sträng"
	fmt.Println("Längden på strängen är:", len(str))
}
```

Output:
```Längden på strängen är: 20```

Som du kan se använder vi funktionen ```len``` för att få längden på strängen och sedan printar vi ut resultatet tillsammans med en beskrivande text. Det är väldigt enkelt och det går snabbt att använda denna metoden i dina egna projekt.

## Djupdykning

Längden på en sträng beräknas genom antalet bytes i strängen, inte antalet tecken. Detta betyder att speciella tecken och bokstäver som inte finns i ASCII-alfabetet, som till exempel åäö, kan påverka längden av strängen. Om du behöver hantera icke ASCII-tecken i dina projekt är det viktigt att du är medveten om detta.

## Se även

- [Go Documentation: Strings](https://golang.org/pkg/strings/)
- [A Tour of Go: Strings and Characters](https://tour.golang.org/basics/1)
- [The Go Programming Language Specification: Length and capacity](https://golang.org/ref/spec#Length_and_capacity)