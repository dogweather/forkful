---
title:    "Kotlin: Extrahering av delsträngar"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

I den här bloggposten kommer vi att utforska hur man kan extrahera substrängar i Kotlin. Detta kan vara användbart när du arbetar med strängar och behöver en del av en sträng istället för hela. Låt oss ta en titt på hur man kan göra detta och varför det kan vara användbart.

## Hur man gör

För att extrahera en substräng i Kotlin, används funktionen `substring` på en sträng. Denna funktion tar två parametrar - startindex och slutindex för den del av strängen som ska extraheras. Båda indexen är nollbaserade, vilket betyder att det första tecknet i strängen har indexet 0.

För att visa hur detta fungerar, låt oss ta en sträng och extrahera en substräng från den:

```Kotlin
val sträng = "Hej från Sverige!"
val substräng = sträng.substring(4,8)
println(substräng)
```

Resultatet av detta kommer att vara "från".

## Djupdykning

Det finns några olika sätt att använda `substring`-funktionen på. Här är några exempel:

- Om endast startindexet anges kommer substrängen att extraheras från det angivna indexet till slutet av strängen.
- Om inget slutindex anges, kommer substrängen att extraheras från det angivna startindexet till slutet av strängen.
- Om slutindexet är större än längden på strängen, kommer substrängen att vara allt från det angivna startindexet till slutet av strängen.

Funktionen `substring` returnerar en ny sträng. Det är viktigt att notera att den ursprungliga strängen fortfarande finns kvar och inte ändras av funktionen.

För att läsa mer om olika sätt att använda `substring`-funktionen, ta en titt på Kotlin dokumentationen.

## Se också

- [Kotlin dokumentationen för substring-funktionen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring.html)
- [En tutorial om strängmanipulering i Kotlin](https://www.raywenderlich.com/4665422-kotlin-strings-and-string-templates-tutorial)
- [En video om strängmanipulering i Kotlin](https://www.youtube.com/watch?v=ZZDOCR1oGVg)