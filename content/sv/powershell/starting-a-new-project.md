---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att starta ett nytt projekt är processen att skapa grunden för kodstruktur, bibliotek och funktionalitet i ett nytt programmeringsarbete. Programmerare gör det för att etablera en stable grund att bygga vidare på och för att säkerställa smidig utveckling.

## Hur man gör:
Här är kodexemplen för projektstart i PowerShell. (Observera: output kan variera beroende på din version av PowerShell och inställningar i omgivningen)

```PowerShell
# Skapa en ny katalog
New-Item -Path 'C:\MittProjekt' -ItemType Directory

# Byta till den nya katalogen
Set-Location -Path 'C:\MittProjekt'

# Skapa en ny PowerShell script fil
New-Item -Path 'C:\MittProjekt' -Name 'mitt-script.ps1' -ItemType File
```

Output kan se ut så här:

```PowerShell
Directory: C:\
Mode                LastWriteTime         Length Name                                                                  
----                -------------         ------ ----                                                                  
d-----        9/24/2021   9:53 PM                MittProjekt      

Path                                                                                                                
----                                                                                                                
C:\MittProjekt\mitt-script.ps1                                                                                      
```

## Djupdykning
PowerShell-datastrukturen skapades primärt för att underlätta administration från kommandoraden och skriptförvaltning på Windows OS. Men på senare år har det breddats för att fungera på flera OS inklusive Linux och MacOS.

Alternativ till att använda PowerShell för projektstarter inkluderar: Bash på Linux-baserade system, Terminal.app på MacOS, eller till och med GUI-baserade system som Visual Studio.

När du går vidare med ett PowerShell-projekt finns det många detaljer att överväga. Allt från hur du strukturerar din skript för att vara återanvändbart och flexibelt, till att bygga så att dina skript är automatiserade och schema-anpassade 

## Se Även
För mer detaljerade resurser om PowerShell, se följande länkar:

- Microsofts officiella dokumentation: [https://docs.microsoft.com/en-us/powershell/](https://docs.microsoft.com/en-us/powershell/)
- Inlärningsresurser för PowerShell: [https://www.powershellmagazine.com/](https://www.powershellmagazine.com/)
- PowerShell GitHub Repo: [https://github.com/PowerShell/PowerShell](https://github.com/PowerShell/PowerShell)