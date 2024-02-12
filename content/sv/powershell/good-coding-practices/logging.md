---
title:                "Loggning"
aliases:
- /sv/powershell/logging.md
date:                  2024-01-26T01:07:23.767722-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggning"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/logging.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Loggning är i grund och botten att lämna ett spår genom din kod - det är hur du håller koll på vad som händer när ditt skript körs i det vilda. Programmerare loggar för att felsöka, för att spåra appbeteende, för att övervaka prestanda och för att hålla ett öga på eventuella skumraskaffärer.

## Hur man gör:
Här är knepen för att strö in grundläggande loggning i dina skript:

```PowerShell
# Skapa ett enkelt loggmeddelande
Write-Host "Info: Startar skriptprocessen."

# Skriva till en fil
"Info: Detta är ett loggat meddelande." | Out-File -Append myLog.log

# Använda den inbyggda cmdleten för mer detaljerad loggning
Start-Transcript -Path "./detailedLog.log"
Write-Output "Varning: Något är inte riktigt rätt."
# ... ditt skript gör grejer
Stop-Transcript

# Utdata från detailedLog.log
******************************
Windows PowerShell transkript start
Starttid: 20230324112347
Användarnamn  : PShellGuru@example.com
Kör som-användare: PShellGuru@example.com
Konfigurationsnamn: 
Maskin  : PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
Värdapplikation: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
Process-ID: 2024
PS-version: 7.1.2
```

Nu finns det ett spel-för-spel av vad din kod har ägnat sig åt i dina loggar.

## Fördjupning:
Historiskt är loggning nästan lika gammal som programmering i sig. Det är som en skeppsdagbok, men för mjukvara. Förr i tiden kan det ha varit utskrifter eller teletypeskrivare; nu handlar det allt om filer och avancerade logghanteringssystem.

När du är nere i PowerShell-skottgraven är `Write-Host` snabbt och smutsigt, men det spottar bara ut text till konsolen, inte så bra för att hålla register. `Out-File` ger dig ett enkelt sätt att slänga text i en fil, men för den riktiga saften så vill du ha `Start-Transcript` och `Stop-Transcript` som loggar allt — inmatning, utmatning, hela köret.

Alternativ? Säkert, om du hanterar företag kanske du tittar på Windows Event Log eller använder mjukvara som Logstash, men för ditt dag-till-dag-skript, håll dig till PowerShell-verktygen. När det gäller genomförande, kom ihåg att logga smart – för lite och det är värdelöst, för mycket och det blir bara brus.

## Se Också:
Kolla in dessa för att få koll på allt som rör loggning i PowerShell:
