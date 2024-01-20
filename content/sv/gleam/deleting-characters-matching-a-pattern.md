---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Ta bort tecken som matchar ett mönster i Gleam

## Vad & Varför?
Att ta bort tecken som matchar ett mönster innebär att definiera ett schema och avlägsna alla tecken som passar in på det. Programmerare använder den här tekniken för att rensa upp data och minska komplexiteten.

## Hur man gör det:
Här är ett enkelt exempel på det i Gleam:

```gleam
import gleam/regex.{replace}

fn main() {
  let pattern = regex.from_string("[a-z]") |> result.unwrap
  let input = "abc123 def456"
  let output = replace(pattern, input, "", _)
  
  case output {
    Ok(v) -> io.println(v)  // will print '123 456'
    Error(_) -> io.println("Något gick fel") 
  }
}
```
I det här exemplet ersätter vi alla små bokstäver i strängen med ingenting, vilket effektivt tar bort dem.

## Djupdykning
Metoden att ta bort tecken som matchar ett mönster har använts sedan tidigt i programmeringshistorien som ett sätt att bearbeta textdata effektivt. Det finns många sätt att implementera det, och i Gleam gör vi det med hjälp av inbyggda regex-verktyg. Ett historiskt alternativ är att använda iterativ bearbetning av strängar, men detta har tenderat att vara mer kodtungt och mindre effektivt.

Gleam implementerar detta genom att först skapa ett RegExp-objekt med det önskade mönstret och sedan använda detta till att ersätta matchande tecken i den givna strängen. Operationen är icke-destruktiv och ger en ny sträng som kan användas på önskat sätt.

## Se även
För mer information, se följande länkar:
- [Gleam's regex documentation](https://gleam.run/stdlib/regex/)
- [A Gentle Introduction to Regex](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Mozilla's Regex guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)