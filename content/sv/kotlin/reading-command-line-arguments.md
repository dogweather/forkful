---
title:    "Kotlin: Läsa inmatade kommandoradsargument"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
Att läsa kommandoradsargument är en viktig del av programmering då det låter dig skapa flexibla program som kan anpassas efter användarens behov.

## Hur man gör
Först och främst behöver vi importera modulen för att läsa kommandoradsargument. Detta görs genom att använda följande kodblock:
```Kotlin
import kotlinx.cli.*
```

När modulen är importerad kan vi använda funktionen `parse` för att läsa argumenten. Detta görs genom att först skapa en instans av `ArgParser` och sedan definiera de argument som vi vill läsa. Här är ett exempel på hur det skulle kunna se ut:
```Kotlin
val parser = ArgParser("Min program")
val numArg by parser.argument(ArgType.Int, description = "Ange ett heltal")
val stringArg by parser.argument(ArgType.String, argumentType = ArgType.Multiple, description = "Ange en eller flera strängar")
parser.parse(args)
```

I det här exemplet har vi skapat en instans av `ArgParser` med titeln "Min program". Vi har sedan definierat två argument, `numArg` som förväntar sig ett heltal och `stringArg` som förväntar sig en eller flera strängar. Slutligen använder vi funktionen `parse` för att läsa in argumenten som användaren matar in via kommandoraden.

För att skriva ut resultatet av våra inlästa argument kan vi använda funktionen `println` tillsammans med variablerna `numArg` och `stringArg`, som visas i exemplet nedan:
```Kotlin
println("Du angav följande heltal: $numArg")
println("Du angav följande strängar: $stringArg")
```
Om vi skulle köra programmet med följande kommandorad:
```
MinProgram -n 123 -s hej hello world
```
så skulle output bli:
```
Du angav följande heltal: 123
Du angav följande strängar: [hej, hello, world]
```

## Deep Dive
Kommandoradsargument är användbara för att göra dina program mer flexibla och anpassningsbara för olika användare. Du kan till exempel använda argument för att välja olika funktioner eller läsa in data från externa filer. Det är också en bra praxis att kontrollera och validera de inlästa argumenten för att undvika fel i ditt program.

En annan användbar funktion i modulen `kotlinx.cli` är möjligheten att lägga till flaggor. Det låter dig ange valfria argument som, om de används, ändrar beteendet hos ditt program. Det finns flera andra användbara funktioner i modulen som tillåter mer komplexa mönster och validering av kommandoradsargument, så se gärna dokumentationen för mer detaljerad information.

## Se också
- [Dokumentation om "kotlinx.cli"](https://github.com/Kotlin/kotlinx.cli)
- [En tutorial på svenska som förklarar hur man läser kommandoradsargument](https://dev.to/indrolie/kotlin-kommandoradsargument-och-styra-programmet-efter-dem-16gj)