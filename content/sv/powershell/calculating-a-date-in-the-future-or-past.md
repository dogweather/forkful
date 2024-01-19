---
title:                "Beräkna ett datum i framtiden eller förfluten"
html_title:           "PowerShell: Beräkna ett datum i framtiden eller förfluten"
simple_title:         "Beräkna ett datum i framtiden eller förfluten"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad och varför?

Beräkning av ett datum i framtiden eller förflutna är praktiken att fastställa ett specifikt datum utifrån ett givet datum plus eller minus ett antal dagar. Programmerare gör detta för att hantera allt från uppgiftsplanering till periodiska och fördröjda händelser.

## Hur man gör:

Här är några exempel på hur du kan beräkna ett datum i framtiden eller förflutna i PowerShell.

```PowerShell
# Lägga till 10 dagar till aktuellt datum
(Get-Date).AddDays(10)

# Dra av 7 dagar från aktuellt datum
(Get-Date).AddDays(-7)
```
När du kör detta ser utdata något ut så här:
```PowerShell
# För dagens datum (2022-07-18)
2022-07-28 14:32:48

# Och för sjunde dagen i framtiden
2022-07-11 14:32:48
```
## Fördjupning

Beräknad datumhantering i PowerShell har förändrats betydligt sedan dess ursprung i Windows Shell skript. Det finns också alternativ för att räkna ut datum, till exempel att använda tredjepartsbibliotek eller komma in i.NET ramverket för mer granulära funktioner.

Implementation av PowerShell-datummatematik är förvånansvärt enkelt tack vare .NET-understrukturen. Funktionen AddDays() som nämnts ovan är i själva verket en integrerad funktion i .NET DateTime-objektet, vilket gör det bekvämt och enkelt för utvecklare att utföra datummatematik.

## Se också

För mer att läsa om datum- och tidsmanipulation i PowerShell, kolla in följande länkar:

1. [Arbeta med datum och tidsobjekt i PowerShell](https://docs.microsoft.com/sv-se/powershell/scripting/samples/working-with-date-and-time-objects?view=powershell-7.1)
2. [Om DateTime-objekt i .NET](https://docs.microsoft.com/sv-se/dotnet/api/system.datetime?view=net-5.0)

Dyka djupare in i dessa ämnen hjälper till att illustrera flexibiliteten och kraften i PowerShell som ett verktyg för både administratörer och utvecklare. Lycka till!