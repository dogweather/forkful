---
title:                "Beräkning av ett datum i framtiden eller förflutna"
html_title:           "PowerShell: Beräkning av ett datum i framtiden eller förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller förflutna"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Att beräkna ett datum i framtiden eller förfluten tid är en vanlig uppgift för programmerare. Det kan vara användbart i många olika situationer, som att planera scheman, automatisera uppgifter eller skapa påminnelser.

Varför gör programmerare detta? För att undvika manuellt arbete och öka effektiviteten. Genom att använda beräknade datum kan man spara tid och minimera risken för fel.

# Hur man:

Att beräkna ett datum i framtiden eller förfluten tid är enkelt med hjälp av PowerShell. Här är några exempel på hur du kan göra det och vilket resultat du kan förvänta dig:

## Beräkna ett datum i framtiden: 

```PowerShell
$date = (Get-Date).AddDays(5)
$date.ToString("yyyy-MM-dd")
```

Output: 2021-04-25

I det här exemplet använder vi cmdleten "AddDays" för att lägga till 5 dagar till dagens datum. Vi använder sedan "ToString" för att formatera resultatet till önskad format.

## Beräkna ett datum i förfluten tid: 

```PowerShell
$date = (Get-Date).AddYears(-2)
$date.ToString("MM/dd/yyyy")
```

Output: 04/20/2019

Här använder vi samma teknik som i det förra exemplet men med cmdleten "AddYears" för att ta bort 2 år från dagens datum och sedan formatera resultatet.

## Använda specifika datumvärden: 

```PowerShell
$date = Get-Date -Year 2022 -Month 12 -Day 31
$date.ToString("dddd, dd MMMM yyyy")
```

Output: Saturday, 31 December 2022

I det här exemplet använder vi cmdleten "Get-Date" med specifika värden för år, månad och dag för att skapa ett datum. Vi formaterar sedan resultatet till önskat format med "ToString".

# Deep Dive

Att beräkna datum är inte något nytt, det har gjorts sedan långt tillbaka i tiden då det användes för att planera jordbruk och ceremonier. Idag finns det olika sätt att utföra denna uppgift, men i PowerShell använder man oftast cmdleten "Add" för att lägga till och "Subtract" för att ta bort från ett datum.

Det finns också andra alternativ, som att använda .NET Frameworks DateTime-metoder eller skapa egna funktioner i PowerShell. Det viktigaste är att hitta det sätt som passar bäst för din specifika uppgift och behov.

# See Also

- [PowerShell Add Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.add?view=net-5.0)
- [PowerShell Subtract Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.subtract?view=net-5.0)
- [DateTime Structure in .NET](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)