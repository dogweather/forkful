---
title:                "Interpolera en sträng"
date:                  2024-01-20T17:51:16.255715-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolera en sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Interpolering av strängar innebär att du stoppar in variabler eller uttryck direkt inuti en sträng. Programmerare använder det för att smidigt bygga dynamiska meddelanden eller strukturerad text.

## How to:
Interpolering i Gleam görs med hjälp av `string.concat` eller genom att omvandla variabler till strängar och sammanfoga dem. Inget inbyggt teckensnitt som `"Hello, \(name)!"` ännu, men vi klarar oss med `string.concat` och `int.to_string` för siffror. Exempel:

```gleam
fn main() {
  let name = "Världen"
  let age = 7
  let greeting = string.concat(["Hej ", name, "! Du är ", int.to_string(age), " år gammal."])
  io.println(greeting)
}
```

Kör koden och utdata blir: `Hej Världen! Du är 7 år gammal.`

## Deep Dive
String-interpolering är inte en uråldrig konst. Det steg fram i takt med behovet av att göra strängmanipulation enklare. I språk som JavaScript eller Ruby är det enormt förenklat med syntax som `"Hello, #{name}!"`. Gleam är inspirerat av Erlang och har stark typning, så en direkt interpolering finns inte än. Tills vidare använder vi `string.concat` eller `Int.to_string` för att omvandla tal till strängar, och `++` för att sammanfoga listor av strängar.

## See Also
För en fördjupning i strängmanipulation i Gleam, kolla in de officiella dokumenten:

Utforska även forum och community för senaste diskussionerna och uppdateringarna:
- Gleam på Github: [github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
