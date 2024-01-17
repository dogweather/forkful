---
title:                "Kontrollera om en mapp finns"
html_title:           "PowerShell: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att kontrollera om en mapp existerar är en vanlig uppgift för många programmerare. Det innebär helt enkelt att man kollar om en viss mapp finns tillgänglig på en dator eller server. Detta kan vara användbart när man ska utföra olika åtgärder, som att manipulera filer eller skapa nya mappar.

## Hur man gör:

Kontrollera om en mapp existerar med hjälp av PowerShell är enkelt. Använd kommandot "Test-Path" och ange sökvägen till mappen du vill kolla. Om mappen finns så returnerar kommandot "True", annars blir det "False".

```PowerShell
Test-Path C:\Users\John\Documents
```

Detta kommer att kolla om mappen "Documents" finns i John's dokumentmapp och returnera True om den gör det.

Om du vill använda resultatet av denna kontroll i en if-sats eller en annan kodblock, kan du spara resultatet i en variabel:

```PowerShell
$exists = Test-Path C:\Users\John\Documents

if($exists){
    "Mappen finns!"
}
else{
    "Mappen existerar inte."
}
```

Detta kommer att skriva ut antingen "Mappen finns!" eller "Mappen existerar inte." beroende på om mappen finns eller inte.

## Djupdykning:

Förmågan att kontrollera om en mapp existerar har funnits i PowerShell sedan början av dess utveckling. Det är en enkel men användbar funktion som kan underlätta många programmeringsuppgifter.

En alternativ metod för att kontrollera om en mapp existerar är att använda kommandot "Get-ChildItem" och därefter kolla om resultatet innehåller den sökta mappen. Detta kan dock vara en långsammare metod för större mappstrukturer.

När det kommer till implementationen av "Test-Path" i PowerShell så används ett systemanrop för att faktiskt kontrollera om mappen existerar eller inte. Detta gör det snabbt och effektivt att använda, även för större mappstrukturer.

## Se även:

- [PowerShell: Test-Path](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-7)
- [PowerShell: Get-ChildItem](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-childitem?view=powershell-7)