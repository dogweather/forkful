---
title:    "Go: Sammanslagning av strängar"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Ett vanligt problem i programmering är att kombinera flera strängar tillsammans. Med hjälp av Go's inbyggda string concatenation funktion kan du enkelt kombinera flera strängar till en enda, vilket är användbart för hantering av data eller utskrift av text.

## Hur man gör

För att sammanfoga strängar i Go använder vi + operatorn. Låt oss ta ett enkelt exempel:

```Go
str1 := "Hej"
str2 := "världen!"
res := str1 + str2

fmt.Println(res) // result: "Hej världen!"
```
I det här exemplet skapar vi två variabler, "str1" och "str2", som innehåller olika strängar. Sedan använder vi + operatorn för att sammanfoga dem och tilldelar resultatet till variabeln "res". Slutligen skriver vi ut resultatet genom att använda fmt.Println() funktionen.

Vi kan också använda += operatorn för att sammanfoga en befintlig sträng med en annan. Låt oss visa detta med ett exempel:

```Go
str3 := "Hello"
str3 += " world!"

fmt.Println(str3) // result: "Hello world!"
```

Som du kan se i det här exemplet lägger vi till " world!" till den befintliga strängen "Hello" genom att använda += operatorn.

## Djupdykning

När vi sammanfogar strängar i Go, skapar vi egentligen en ny sträng och inte faktiskt ändrar på de befintliga strängarna. Detta beror på att strängar är oföränderliga i Go, vilket innebär att de inte kan ändras. Istället skapas en ny sträng som innehåller det kombinerade innehållet av de två ursprungliga strängarna.

En annan viktig sak att notera är att prestandan påverkas när man sammanfogar strängar, särskilt när man kombinerar många strängar på en gång. Detta beror på att varje gång en sträng sammanfogas, måste en ny sträng skapas vilket kan vara en resurskrävande process. Så om du behöver sammanfoga många strängar, bör du överväga att använda en bytes.Buffer() för att förbättra prestandan.

## Se också
- [Go Strings](https://golang.org/ref/spec#String_types)
- [Go fmt.Println() funktionen](https://golang.org/pkg/fmt/#Println)
- [Go bytes.Buffer typen](https://golang.org/pkg/bytes/#Buffer)