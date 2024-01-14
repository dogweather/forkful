---
title:    "Gleam: Ta bort tecken som matchar ett mönster"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför

I denna bloggpost kommer vi att ta en titt på hur man kan radera tecken som matchar ett visst mönster i Gleam-programmering. Denna teknik kan vara användbar för att rensa upp data eller för att manipulera strängar på ett mer effektivt sätt.

## Så här gör du

För att radera tecken som matchar ett mönster i Gleam kan du använda funktionen `delete` tillsammans med ett reguljärt uttryck. Här är ett exempel på en sträng som innehåller olika punktuationstecken som ska tas bort:

```Gleam
import gleam/re

let text = "Det här, är en? sträng! med, en massa. punktuation:stecken."

let re = Regex.compile(~pattern="\p{P}+", ~flags=Regex.ignore_case())

text
|> String.replace(~pattern=re, ~replacement="")
|> IO.inspect // "Det här är en sträng med en massa punktuationstecken."
```

I detta exempel använder vi `String.replace`, som tar in en sträng, ett reguljärt uttryck och en ersättningssträng. I vårt fall är ersättningssträngen tom, vilket leder till att alla tecken som matchar mönstret tas bort från den ursprungliga strängen.

## Djupdykning

Ett reguljärt uttryck, eller regex, är en vanlig metod för att matcha och manipulera textsträngar. Detta mönstermatcher kan vara mycket kraftfulla och användbara, men det kan också vara knepigt att förstå och skapa dem. Om du vill lära dig mer om reguljära uttryck kan du kolla in [denna guide](https://regexone.com/) eller [Gleams dokumentation](https://gleam.run/documentation/guide-regular-expressions.html).

## Se även

- [Gleams `String`-modul](https://gleam.run/documentation/stdlib-String.html)
- [Dokumentation för `String.replace`](https://gleam.run/documentation/stdlib-String-String.replace.html)
- [Regex-dokumentation för Gleam](https://gleam.run/documentation/stdlib/Regex.html)