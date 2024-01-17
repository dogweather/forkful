---
title:                "Jämföra två datum"
html_title:           "PowerShell: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att jämföra två datum är en vanlig uppgift för programmerare. Det är ett sätt att kontrollera och hantera information om när olika händelser inträffade eller ska inträffa. Det kan vara användbart för att hantera olika tidsbaserade uppgifter som schemaläggning, övervakning eller rapportering.

## Hur man:

```PowerShell
$datum1 = Get-Date "2021-01-01"
$datum2 = Get-Date "2021-02-15"

# Jämför om datum1 ligger före datum2
$datum1 -lt $datum2 # true

# Jämför om datum1 ligger efter datum2
$datum1 -gt $datum2 # false

# Jämför om datum1 och datum2 är lika
$datum1 -eq $datum2 # false
```

## Fördjupning:

Att jämföra datum har varit en viktig del av programmering sedan lång tid tillbaka. Det kan göras på flera olika sätt, beroende på vilken plattform eller programmeringsspråk man använder. I PowerShell används operatörerna -lt (less than), -gt (greater than) och -eq (equal) för att utföra jämförelser mellan datum. Det finns också möjligheten att använda .NET Framework funktioner för mer avancerade jämförelser.

## Se även:

- https://docs.microsoft.com/sv-se/powershell/scripting/learn/deep-dives/everything-about-dates-and-times
- https://docs.microsoft.com/sv-se/dotnet/api/system.datetime?view=net-5.0