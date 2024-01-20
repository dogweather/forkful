---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Slumpmässiga nummer används överallt i programmeringsvärlden. De används för att indikera slumpmässiga händelser, som matbitar som dyker upp i ett Pac-Man-spel, men de kan också välja en vinnande post bland miljontals, som i ett lotteri.

## Hur gör man:

Här är ett exempel på hur du genererar ett slumpmässigt tal i PowerShell.

```PowerShell
$random = New-Object System.Random
$randomNumber = $random.Next(1, 100)
Write-Host $randomNumber
```

När du kör ovanstående kod kan output se ut så här:

```PowerShell
45
```

```PowerShell
Get-Random -Minimum 1 -Maximum 100
```

När du kör ovanstående kod kan output se ut så här:

```PowerShell
92
```
 
## Djupdykning

Slumpmässiga tal har en lång historia inom datorvetenskap och programmering. När programmerare först började generera slumpmässiga tal i kod var det en ganska komplicerad process. Men med utvecklingen av moderna programmeringsspråk har processen blivit mycket enklare och mer intuitiv. 

När det gäller att generera slumpmässiga nummer i PowerShell speglar tekniken det som görs i .NET Framework. Det finns andra metoder, till exempel `Get-Random` cmdlet som vi tidigare såg, vilket är en inbyggd funktion i PowerShell.

Det är viktigt att notera att dessa nummer inte är riktigt "slumpmässiga". De genereras av en algoritm med en uppsättning fast input, vilket betyder att de i själva verket är "pseudo-slumpmässiga" nummer. Även om de passar för de flesta ändamål, kan de vara olämpliga där verklig slumpmässighet krävs, som för vissa typer av krypteringar.

## Se Även

- För mer om skapandet av slumpmässiga nummer i andra programmeringsspråk, se [Random numbers in programming](https://en.wikipedia.org/wiki/Random_number_generation)

- Grundläggande PowerShell-tutorial: Microsoft’s [PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1)

- För mer om pseudo-slumpmässiga nummer, se [Pseudorandom number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)