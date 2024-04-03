---
date: 2024-01-26 01:07:23.767722-07:00
description: "Hur man g\xF6r: H\xE4r \xE4r knepen f\xF6r att str\xF6 in grundl\xE4\
  ggande loggning i dina skript."
lastmod: '2024-03-13T22:44:38.132332-06:00'
model: gpt-4-1106-preview
summary: "H\xE4r \xE4r knepen f\xF6r att str\xF6 in grundl\xE4ggande loggning i dina\
  \ skript."
title: Loggning
weight: 17
---

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
