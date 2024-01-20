---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa en textfil innebär att ta informationen lagrad i filen och förstå den i programmet. Detta är viktigt för programmerare, eftersom program ofta behöver interagera med data som inte är hårdkodade.

## Hur man gör:

Låt oss starta PowerShell och utforska hur man läser från en textfil. 

```PowerShell
# Skapa en variabel med filens sökväg
$filePath = "C:\temp\myfile.txt"

# Använd Get-Content för att läsa filen
$text = Get-Content $filePath

# Skriv ut texten till konsolen
$text
```

Om det finns en fil vid den specificerade sökvägen kommer skriptet att skriva ut innehållet i denna fil.

## Fördjupning:

Historiskt sett har PowerShell alltid förlitat sig på Get-Content cmdleten för att läsa textfiler. Den är både kraftfull och flexibel, men det finns också alternativ. Till exempel kan du använda System.IO.File-klassen i .NET Framework om du vill ha mer kontroll över hur filen läses.

I själva verket skapar Get-Content en array där varje rad i filen blir ett element. Denna process kan vara långsam för stora filer. Om du istället vill läsa filen lagledare, anser man använda `-ReadCount`-flaggan eller en "StreamReader"-instans från .NET Framework.

## Se Även:

Kolla in följande länkar för mer om ämnet:

- PowerShell-dokumentationen om Get-Content: [https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1)
- Microsofts .NET dokumentation om System.IO.File: [https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=net-5.0](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=net-5.0)