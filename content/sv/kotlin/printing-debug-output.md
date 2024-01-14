---
title:                "Kotlin: Utmatning av felsökningsresultat"
simple_title:         "Utmatning av felsökningsresultat"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Felsökning är en viktig del av programmering som hjälper till att hitta och lösa problem i koden. Genom att skriva ut debug-utskrifter kan man enkelt få insikt i vad som händer i programmet vid olika punkter under körningen.

## Så här gör du

För att skriva ut debug-utskrifter i Kotlin kan man använda en enkel inbyggd funktion kallad `println()`. Denna funktion tar emot ett värde som argument och skriver ut det till konsolen. Till exempel:

```Kotlin
val name = "Johan"
val age = 30
println("Hej, mitt namn är $name och jag är $age år gammal.")
```

Detta kommer att skriva ut följande i konsolen:

```
Hej, mitt namn är Johan och jag är 30 år gammal.
```

Man kan också skriva ut flera värden genom att separera dem med kommatecken inom samma `println()` funktion. Till exempel:

```Kotlin
val num1 = 10
val num2 = 5
println("Summan av $num1 och $num2 är ${num1 + num2}.")
```

Detta kommer att skriva ut följande:

```
Summan av 10 och 5 är 15.
```

## Djupdykning

För att få mer detaljerad information om vad som händer i programmet kan man använda funktionen `print()` istället för `println()`. Skillnaden är att `print()` inte lägger till en ny rad efter utskriften, vilket innebär att flera utskrifter kommer att visas på samma rad. Till exempel:

```Kotlin
val num1 = 10
val num2 = 5
print("Nummer 1 är $num1, ")
print("Nummer 2 är $num2 och ")
print("summan är ${num1 + num2}.")
```

Detta kommer att skriva ut följande:

```
Nummer 1 är 10, Nummer 2 är 5 och summan är 15.
```

Det är också möjligt att formatera utskriften för att göra den mer läslig. Detta kan göras genom att använda `println()` och `print()` med formattecken som `%s` för strängar, `%d` för heltal, `%f` för flyttal och så vidare. Till exempel:

```Kotlin
val name = "Sara"
val age = 25
val height = 1.65
println("%s är %d år gammal och är %.2f meter lång.".format(name, age, height))
```

Detta kommer att skriva ut följande:

```
Sara är 25 år gammal och är 1.65 meter lång.
```

## Se även

För mer information om felsökning i Kotlin och användningen av `println()` och `print()`, besök gärna följande länkar:

- https://kotlinlang.org/docs/reference/basic-types.html#string-templates
- https://www.jetbrains.com/help/idea/debugging.html#debugConsole
- https://www.baeldung.com/kotlin-printing-debug-output