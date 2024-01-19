---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att jämföra två datum innebär att avgöra om ett datum är tidigare, senare eller identiskt med ett annat datum. Programmers gör detta för att utföra tidbaserade operationer, som att sortera händelser eller beräkna tidsskillnaden.

## Hur göra:

PowerShell-funktionen som används för att jämföra datum är "`Compare-Object`". Nedan är ett exempel:

```PowerShell
# Defining the dates
$Date1 = Get-Date -Year 2020 -Month 1 -Day 1
$Date2 = Get-Date -Year 2020 -Month 12 -Day 31

#Comparing the dates
$Result = Compare-Object -ReferenceObject $Date1 -DifferenceObject $Date2

#Displaying the result
$Result
```
När du kör ovanstående kod kommer du att se ett resultat som liknar detta:

```PowerShell
InputObject SideIndicator
----------- -------------
31 december 2020 23:59:59 =>
```

Detta indikerar att `$Date2` ("`31 december 2020`") är senare än `$Date1`.

## Djupdykning:

Även om PowerShell infördes först 2006, är jämförelsen mellan två datum en standarduppgift i programmering. Funktionen "`Compare-Object`" i PowerShell är bara en av många tekniker som du kan använda.

Du kan också använda jämförelseoperatorer direkt, till exempel "`-gt`" (större än) eller "`-lt`" (mindre än). Skriptet nedan ger samma resultat:

```PowerShell
# Defining the dates
$Date1 = Get-Date -Year 2020 -Month 1 -Day 1
$Date2 = Get-Date -Year 2020 -Month 12 -Day 31

#Comparing the dates
if ($Date2 -gt $Date1) { 
   "Date2 is later than Date1" 
} else { 
   "Date1 is later than Date2" 
}
```
## Se Även:

Om du vill veta mer om att arbeta med datum i PowerShell, kolla följande länkar:

- Microsofts officiella dokumentation om [`Get-Date`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [`Compare-Object`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/compare-object?view=powershell-7.1)
- En bra artikel om datumanvändning i PowerShell på [`4SysOps`](https://4sysops.com/archives/use-powershell-to-work-with-dates-and-times/)