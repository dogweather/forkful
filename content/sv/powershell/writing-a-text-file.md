---
title:                "Skriver en textfil."
html_title:           "PowerShell: Skriver en textfil."
simple_title:         "Skriver en textfil."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att skriva en textfil innebär att vi skapar en vanlig textfil med kod som kan läsas och förstås av både människor och datorer. Detta är en viktig del av programmering eftersom det hjälper oss att lagra och hantera information på ett effektivt sätt.

## Hur man:

```PowerShell
# Skapa en ny textfil
New-Item -Path C:\Users\ExampleUser\Documents\example.txt -ItemType File

# Skriv till en befintlig textfil
Add-Content -Path C:\Users\ExampleUser\Documents\example.txt -Value "Detta är ett exempel på text som läggs till i filen."

# Läs från en textfil
Get-Content -Path C:\Users\ExampleUser\Documents\example.txt
```

Output:

```
Detta är ett exempel på text som läggs till i filen.
```

## Djupdykning:

Att skriva en textfil har funnits sedan de tidiga dagarna av programmering för att lagra och hantera data på ett strukturerat sätt. Det finns även alternativa sätt att skriva till en fil, såsom att använda kommandot `Set-Content`, men `Add-Content` gör det möjligt för oss att lägga till innehåll i en befintlig fil utan att skriva över den befintliga informationen.

För att skriva till en textfil kan vi använda olika parametrar för att ändra formatet, såsom att välja att lägga till texten i början eller slutet av filen, samt att ange filens teckenkodning.

## Se även:

- [PowerShell dokumentation för New-Item](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/new-item?view=powershell-7.1)
- [PowerShell dokumentation för Add-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/add-content?view=powershell-7.1)
- [PowerShell dokumentation för Get-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1)