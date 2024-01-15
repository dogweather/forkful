---
title:                "Radera tecken som matchar ett mönster"
html_title:           "Gleam: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
Det finns många användbara fall där man kan behöva ta bort tecken som matchar ett visst mönster från en sträng. Till exempel när man rensar ut onödiga specialtecken från en text eller när man måste ta bort alla siffror från en sträng.

## Så här gör du
För att ta bort karaktärer baserat på ett givet mönster i Gleam, kan du använda funktionen `string.replace`, som tar emot ett mönster och en ersättningstext. Till exempel:

```Gleam
import gleam/string

let text = "123-456-789"
let pattern = "[0-9]"
let result = string.replace(text, pattern, "")
```

I detta exempel ersätts alla siffror i strängen med en tom sträng, vilket resulterar i att den nya strängen blir "----". Här är ett annat exempel där vi bara vill ta bort alla versaler från en sträng:

```Gleam
import gleam/string

let text = "Hello World"
let pattern = "[A-Z]"
let result = string.replace(text, pattern, "")
```

Resultatet av detta kodblock blir "ello orld". 

## Djupdykning
När man använder `string.replace` för att ta bort tecken baserat på ett mönster, kan man också utnyttja reguljära uttryck för att göra mönstret mer avancerat. Till exempel, om vi vill ta bort alla vokaler från en sträng kan vi använda följande mönster: `"[aeiouy]"`. Detta kommer att matcha alla vokaler i engelska alfabetet och ta bort dem från strängen.

Det är också viktigt att tänka på att `string.replace` endast tar bort de karaktärer som matchar mönstret. Om du till exempel anger mönstret `"[0-9]"` men inte specificerar en ersättningstext, kommer funktionen bara att ta bort siffrorna och behålla alla andra tecken i strängen.

## Se även
- Gleams officiella dokumentation för `string.replace`: [https://gleam.run/core/string.html#replace](https://gleam.run/core/string.html#replace)
- En utförlig guide för reguljära uttryck i Gleam: [https://medium.com/@matthewgallienne/regular-expressions-in-gleam-f3a657767535](https://medium.com/@matthewgallienne/regular-expressions-in-gleam-f3a657767535)