---
title:                "Utvinning av substrängar"
html_title:           "Go: Utvinning av substrängar"
simple_title:         "Utvinning av substrängar"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/extracting-substrings.md"
---

{{< edit_this_page >}}

##Varför

Är du trött på att behöva hantera hela strängar när du bara behöver en del av den? Att utvinna substrängar är en användbar teknik för att plocka ut specifika delar av en sträng och göra din kod mer effektiv.

##Hur man gör

För att extrahera en substräng i Go använder du funktionen `string[start:end]`, där `start` är indexet för den första tecknet i den önskade substrängen och `end` är indexet för det sista tecknet plus ett. Här är ett exempel:

```Go
text := "Hej världen!"
substring := text[4:9]
fmt.Println(substring) // output: värld
```

I detta exempel har vi använt `string[start:end]` för att extrahera `värld` från den ursprungliga strängen `Hej världen!` genom att använda index 4 för `v` och index 9 för `n`. Det är viktigt att notera att indexet börjar vid 0 och att det sista indexet är exkluderat.

Du kan också ange enbart `start` eller `end`, vilket kommer att extrahera från början respektive slutet av strängen. Om du bara anger `start` kommer substrängen att extraheras från `start` till slutet av strängen och om du bara anger `end` kommer substrängen att extraheras från början till `end-1`. Här är två exempel:

```Go
substring1 := text[4:] // output: världen!
substring2 := text[:3] // output: Hej
```

Det är också möjligt att extrahera enstaka tecken genom att ange enskilda index, till exempel `text[0]` kommer att extrahera det första tecknet i strängen `H`.

##Djupdykning

När du extraherar substrängar i Go används Unicode-kodpunkter för att bestämma indexeringen. Detta innebär att index inte alltid nödvändigtvis motsvarar enstaka tecken, särskilt för icke-latinska tecken. I sådana fall kan `string[start:end]` extrahera tecken som inte är avsedda. Som en lösning kan du använda `rune` och `len` för att få rätt indexering av en sträng som innehåller icke-latinska tecken. Här är ett exempel:

```Go
text := "こんにちは!"
runes := []rune(text)
substring := string(runes[3:5])
fmt.Println(substring) // output: には
```

I detta exempel har vi använt `rune` för att omvandla strängen till en slice av Unicode-kodpunkter och sedan använde vi `len` för att hitta rätt indexering av substringen `には`.

##Se även

- [Official Go Documentation](https://golang.org/doc/)
- [Go Code Examples](https://gobyexample.com/)