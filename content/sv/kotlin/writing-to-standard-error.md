---
title:                "Kotlin: Skriva till standardfel"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfel, eller "standard error", är ett vanligt förekommande sätt att hantera fel i programmeringsspråket Kotlin. När du skriver till standardfel är det viktigt att förstå både varför och hur du ska göra det på rätt sätt. I denna bloggpost kommer vi att utforska varför man bör skriva till standardfel och ge steg-för-steg-instruktioner om hur du gör det.

## Hur man gör det

För att skriva till standardfel i Kotlin använder vi en funktion som heter "System.err.println()". Det är en inbyggd funktion som låter oss skriva ut ett meddelande till standardfel.
 
Ett enkelt exempel på detta är:

```Kotlin
System.err.println("Detta är ett felmeddelande")
```

Detta kommer att skriva ut meddelandet "Detta är ett felmeddelande" till standardfel. En annan viktig aspekt att tänka på är att detta uttryck inte bör kringgärdas av någon try/catch-block. Om vi exempelvis skriver:

```Kotlin
try {
    System.err.println("Detta är ett felmeddelande")
} catch (e: Exception) {
    println(e.message)
}
```

Kommer inte meddelandet att skrivas ut till standardfel. Istället catch-blocket kommer att fånga felet och skriva ut det på konsolen.
 
## Utforska mer

För att lära dig mer om att skriva till standardfel i Kotlin kan du kolla in dokumentationen på [Kotlins hemsida](https://kotlinlang.org/docs/tutorials/command-line.html#command-line-tools-and-runtime-options). Där hittar du till exempel mer information om hur du formaterar dina utskrifter och hur du hanterar olika typer av fel.

## Se även

- [KotlinDocumentation](https://kotlinlang.org/docs/tutorials/command-line.html#command-line-tools-and-runtime-options)
- [KotlinForum](https://discuss.kotlinlang.org/c/libraries/frameworks-tools)
- [KotlinGitHub](https://github.com/JetBrains/kotlin)