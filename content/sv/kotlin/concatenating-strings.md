---
title:    "Kotlin: Sammanslagning av strängar"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanslå strängar är en viktig del av programmering, eftersom det gör det möjligt att kombinera flera olika strängar till en enda och därmed skapa mer dynamiska och läsbara texter i våra program. Det kan vara till nytta när du behöver skriva ut data eller skapa meddelanden för användaren.

## Så här gör du

För att sammanslå strängar i Kotlin, används operatorn "plus" (+) mellan två eller flera strängar. Detta kan göras på flera sätt, beroende på vad du vill uppnå. Nedan finns några exempel på hur man kan sammanfoga strängar i Kotlin, tillsammans med det förväntade resultatet.

```Kotlin
val förnamn = "Maria"
val efternamn = "Larsson"

val namn = förnamn + " " + efternamn
println(namn) // Output: Maria Larsson

val hälsning = "Hej " + namn + ", välkommen till min blogg!"
println(hälsning) // Output: Hej Maria Larsson, välkommen till min blogg!

val tal1 = 10
val tal2 = 5

val summa = "Summan av " + tal1 + " och " + tal2 + " är " + (tal1 + tal2)
println(summa) // Output: Summan av 10 och 5 är 15
```

Som du kan se i exemplen ovan, kan du använda operatorn "+" för att kombinera både strängar och andra typer av variabler. Observera att operatorn "+" alltid måste användas mellan två strängar, annars kommer det att uppstå ett fel.

## Djupdykning

När du kombinerar strängar i Kotlin, skapas en ny sträng varje gång som operatorn "+" används. Detta kan ha en påverkan på prestandan i stora och komplexa program. För att undvika detta, kan du använda funktionen "StringBuilder", som finns inbyggd i Kotlin's standardbibliotek.

StringBuilder erbjuder bättre prestanda vid sammanslagning av strängar, eftersom den skapar och manipulerar en enda sträng istället för att skapa flera nya strängar. Detta kan vara särskilt användbart om du behöver sammanfoga många strängar inuti en for-loop eller i andra prestandakänsliga delar av koden.

Här är ett exempel på hur du kan använda StringBuilder för att sammanfoga strängar:

```Kotlin
val förnamn = "Erik"
val efternamn = "Andersson"

val namn = StringBuilder().append(förnamn).append(" ").append(efternamn).toString()
println(namn) // Output: Erik Andersson
```

Som du kan se i exemplet ovan, bygger vi upp en enda sträng med hjälp av metoden "append" istället för operatorn "+". När vi är färdiga kallar vi på metoden "toString" för att få den slutgiltiga strängen.

## Se även

- [Dokumentation om strängar i Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Dokumentation om StringBuilder i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/index.html)