---
title:                "Modifiera filer med CLI-engreppskommandon"
date:                  2024-01-26T22:25:06.417017-07:00
model:                 gpt-4-0125-preview
simple_title:         "Modifiera filer med CLI-engreppskommandon"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/modifying-files-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att modifiera filer med hjälp av Kommandoradsgränssnittet (CLI) med enradskommandon i PowerShell handlar om att använda kortfattade kommandon för att redigera, transformera eller uppdatera filer direkt från terminalen. Programmerare gör detta för att snabbt göra ändringar i filer utan att öppna dem i en grafisk redigerare, vilket påskyndar arbetsflödet och möjliggör automatisering av repetitiva uppgifter.

## Hur man gör:

För att ersätta en specifik sträng i en fil kan du använda `Get-Content` och `Set-Content` cmdlets i kombination med `ForEach-Object` cmdlet, så här:

```PowerShell
Get-Content ./example.txt | ForEach-Object { $_ -replace 'oldString', 'newString' } | Set-Content ./example.txt
```

För att lägga till en rad i slutet av en fil kan du använda `Add-Content` cmdlet:

```PowerShell
Add-Content ./example.txt "Det här är den nya raden i slutet av filen."
```

Anta att du vill ta bort tomma rader från en fil. I så fall gör PowerShell det enkelt:

```PowerShell
Get-Content ./example.txt | Where-Object { $_.Trim() -ne '' } | Set-Content ./cleaned_example.txt
```

Och exempel på utdata för att ta bort tomma rader kan enkelt vara innehållet i `cleaned_example.txt` nu utan några av de tomma eller blanksteg-endast raderna som var närvarande i `example.txt`.

## Djupdykning

Kraften i att modifiera filer med CLI enradskommandon i PowerShell är rotad i dess omfattande uppsättning cmdlets, som är byggda på .NET-ramverket, vilket ger det en robust uppsättning funktioner. Denna metod återgår till Unix-filosofin att skapa enkla verktyg som gör ett jobb väl, en princip som PowerShell utökar genom att förse en mångsidig verktygslåda inom ett enda skal.

Alternativ till PowerShell för denna uppgift inkluderar att använda Unix-baserade verktyg som `sed`, `awk` eller `grep` i miljöer som Bash. Dessa verktyg är mycket effektiva och har varit lösningen för filmanipulering i Unix/Linux-system i årtionden. PowerShells tillvägagångssätt integreras dock tätt med Windows Object Model, vilket ger en unik fördel i Windows-miljöer.

En viktig genomförandedetalj att notera är att PowerShell bearbetar filinnehåll i minnet, vilket gör det mindre effektivt för mycket stora filer jämfört med vissa strömbaserade verktyg i Unix/Linux. Dessutom kan PowerShells verbositet, även om detta gör skript läsbara, ibland leda till längre enradskommandon jämfört med deras Unix-motsvarigheter. Dock, för Windows-centrerade miljöer och uppgifter som drar nytta av en djup integration med Windows-ekosystemet, erbjuder PowerShell oöverträffade möjligheter.

## Se även

För vidare läsning och mer komplexa exempel på filmanipulering i PowerShell, kan du finna följande resurser hjälpsamma:

- Den officiella PowerShell-dokumentationen, som ger en omfattande guide till dess cmdlets: [https://docs.microsoft.com/en-us/powershell/](https://docs.microsoft.com/en-us/powershell/)
- "PowerShell Scripting Guide" av Ed Wilson, som erbjuder djupgående diskussioner och exempel på skriptning, inklusive uppgifter för filmanipulering.
- För dem som är intresserade av korskompatibilitet eller kommer från en Unix-bakgrund, är "Learning PowerShell for Linux Admins" en utmärkt resurs för att förstå PowerShells kraft över olika operativsystem.
