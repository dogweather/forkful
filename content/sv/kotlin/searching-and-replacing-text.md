---
title:    "Kotlin: Sökning och ersättning av text"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Varför

Ibland när du arbetar med en kodbas, kan du behöva göra stora förändringar i texten. Istället för att gå igenom varje fil manuellt, kan du använda sök-och-ersätt-funktionen för att göra denna process snabbare och mer effektiv. Detta kan spara dig mycket tid och ansträngning när du arbetar med stora mängder text.

## Hur man gör det

För att använda söka-och-ersätta-funktionen i Kotlin, kan du använda följande kod:

```Kotlin
var text = "Hej världen!"
println(text.replace("Hej", "Hello"))
```

Detta kommer att ersätta ordet "Hej" med "Hello" i texten och skriva ut "Hello världen!" i konsolen. Du kan också använda regex för att söka och ersätta specifika mönster i texten. Till exempel:

```Kotlin
var text = "Detta är en text med siffror 123"
println(text.replace(Regex("\\d+"), "123456"))
```

Detta kommer att ersätta alla siffror i texten med "123456" och skriva ut "Detta är en text med siffror 123456" i konsolen.

## Djupdykning

Söka-och-ersätta-funktionen i Kotlin har många olika alternativ och möjligheter. Till exempel kan du använda "replaceFirst" för att bara ersätta den första förekomsten av ett ord eller ett mönster. Du kan också använda "replaceAfter" och "replaceBefore" för att bara ersätta text efter eller innan ett visst ord eller mönster. För mer information om de olika syntaxen och alternativen som finns tillgängliga för sökning och ersättning i Kotlin, kan du kolla in dokumentationen på Kotlin hemsida.

## Se även

- [Kotlin dokumentation om söka och ersätta](https://kotlinlang.org/docs/reference/strings.html#string-representation)
- [Regular expression tutorial](https://www.regular-expressions.info/)
- [Kotlin officiella hemsida](https://kotlinlang.org/)