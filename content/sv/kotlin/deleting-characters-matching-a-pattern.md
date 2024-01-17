---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Kotlin: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort tecken som matchar ett mönster är en vanlig programmeringsuppgift som kan utföras på många olika språk, inklusive Kotlin. Detta innebär att söka igenom en sträng och ta bort alla tecken som stämmer överens med ett givet mönster. Detta kan vara användbart när man vill ta bort icke-önskade tecken från en sträng eller filtrera bort specifika tecken från en inmatad data.

## Hur gör man?
Det finns flera sätt att åstadkomma detta på Kotlin. Ett enkelt sätt är att använda metoden ```removeAll``` som returnerar en kopia av en sträng utan de specifika tecknen som matchar mönstret. Ett annat sätt är att använda regex (regular expressions) för att söka och ta bort de matchande tecknen. Här är ett exempel på båda metoderna:

```Kotlin
//Metoden removeAll
val str = "Detta är en teststräng."
val nyStr = str.removeAll { it == 't' || it == 'ä' }
println(nyStr) //Resultatet blir "Dea är en esräng."

//Regex
val str = "Detta är en teststräng."
val nyStr = str.replace(Regex("[tä]"), "")
println(nyStr) //Resultatet blir "De är en srg."
```

## Utforska djupet
Att ta bort tecken som matchar ett mönster är en av de grundläggande strängbehandlingsuppgifterna inom programmering. Historiskt sett användes det främst för att filtrera bort tecken som inte stöds av ett visst teckenuppsättning (t.ex. borttagning av diakritiska tecken på äldre system). Numera kan det också användas för att rensa en sträng på icke-önskade tecken som t.ex. siffror eller specialtecken. Istället för att ta bort tecken, kan man också använda metoden ```replace``` för att byta ut de matchande tecknen mot annan text.

## Se även
Kotlins dokumentation för strängbehandling: <https://kotlinlang.org/docs/reference/basic-types.html#strings>
Regex for Kotlin: <https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html>