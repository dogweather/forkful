---
title:                "Att läsa kommandoradsargument"
html_title:           "PowerShell: Att läsa kommandoradsargument"
simple_title:         "Att läsa kommandoradsargument"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Vad och varför?
Att läsa kommandoradsargument är en process där en datorprogram visar att den är redo att ta emot instruktioner eller input från en användare genom kommandoraden eller terminalen. Detta är användbart för att utföra uppgifter på ett effektivt sätt och ge en användare möjlighet att anpassa eller ändra en programkörsvariabel i realtid.

# Så här gör du:
```PowerShell
# Exempel på hur man läser ett kommandoradsargument:
param(
    [string]$inputArgument = "Default value"
)

Write-Host "Värdet på inputArgument är: $inputArgument"
```
I det här exemplet skapar vi en ny variabel `inputArgument` och tilldelar den ett standardvärde om inget annat värde ges med vid körning av skriptet. Sedan skrivs värdet av variabeln ut till terminalen.

# Deep Dive
## Historisk kontext:
Läsning av kommandoradsargument är en vanlig företeelse inom programmering, särskilt inom skriptspråk som PowerShell. Det är en process som utvecklades för att göra det enklare för användare att anpassa och styra ett program från kommandoraden istället för att behöva göra ändringar direkt i källkoden.

## Alternativ:
Det finns flera andra sätt att få input från användare, som att läsa från en textfil eller använda en grafisk användargränssnitt (GUI). Dock är kommandoradsargument ett snabbt och effektivt sätt att ge input till ett program, särskilt när det gäller enklare uppgifter eller skript.

## Implementation:
Läsning av kommandoradsargument sker vanligtvis genom att använda parametern `param` följt av en lista av variabler som programmet ska läsa in och tilldela värden till. Det är också vanligt att ge dem standardvärden, vilket ger en användare möjlighet att ändra värdet om de vill.

# Se även:
* [Making Use of Command Line Arguments in PowerShell](https://www.petri.com/making-use-command-line-arguments-powershell)
* [PowerShell param Statement Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7)