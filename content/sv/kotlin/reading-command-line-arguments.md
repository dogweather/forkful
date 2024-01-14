---
title:    "Kotlin: Läsning av kommandoradsargument"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Varför

Att läsa kommandoradsargument är en viktig del av Kotlin-programmering. Genom att kunna hantera kommandoradsinmatning kan du skriva mer flexibla och anpassningsbara program. Dessutom kan det vara användbart när du behöver interagera med användaren eller automatisera uppgifter.

## Så här gör du

För att läsa kommandoradsargument i Kotlin använder vi funktionen `args:Array<String>` som tar emot inmatningen som en matris av strängar. Här är ett enkelt exempel som tar emot en inmatning och skriver ut den till konsolen:

```Kotlin
fun main(args:Array<String>) {
    println(args[0])
}
```

Om du kör detta program i terminalen och anger en inmatning som ett argument kommer den att skrivas ut. Till exempel, om du kör programmet med `kotlin program.kt hej`, kommer det att skriva ut "hej" till konsolen. 

```Kotlin
$ kotlin program.kt hej
hej
```

Du kan också använda en for-loop för att läsa in alla argument och göra något med dem. Till exempel kan vi skriva ut alla argument som en lista:

```Kotlin
fun main(args:Array<String>) {
    for (arg in args) {
        println(arg)
    }
}
```

```Kotlin
$ kotlin program.kt hej jag heter Anna
hej
jag
heter
Anna
```

Du kan också använda andra metoder som `size` för att få antalet argument som skickats in. 

## Utforska djupet

Det finns fler saker du kan göra med kommandoradsargument i Kotlin, som att hantera felaktig eller ofullständig inmatning. Du kan också använda `args` för att passera inmatning till andra funktioner eller klasser.

Det är också viktigt att notera att ordningen på argumenten spelar roll. Om du till exempel har en funktion som tar emot två argument och du matar in dessa argument i omvänd ordning, kommer resultaten att bli felaktiga.

Se även

- [Kotlin - Command line argument handling](https://www.geeksforgeeks.org/kotlin-command-line-argument-handling/)
- [Kotlin Standard Library - Args documentation] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.args/-args/index.html)
- [Learn Kotlin in Y minutes - Command line arguments] (https://learnxinyminutes.com/docs/kotlin/#command-line-arguments)