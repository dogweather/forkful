---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Skapa en tillfällig fil innebär att man gör en icke-varaktig lagringsenhet, ofta för att hålla data under en pågående process. Programmerare gör det för att undvika minnesbelastning och för effektiv datahantering.

## Hur:

Här är ett exempel på hur du skapar och skriver till en tillfällig fil i PowerShell.

```PowerShell
# Skapa en tillfällig fil
$tempFile = [System.IO.Path]::GetTempFileName()

# Skriv till den tillfälliga filen
Set-Content -Path $tempFile -Value "Hej, världen!"

# Kontrollera innehållet i den tillfälliga filen
Get-Content -Path $tempFile
```

När du kör det, här är vad du kan förvänta dig att se:

```PowerShell
Hej, världen!
```

## Djupdykning:

Historiskt sett har skapandet av tillfälliga filer varit ett fundamentalt sätt att hantera temporär data för ett brett spektrum av språk och plattformar, från C till Java till Shell-skript.

Ett alternativ till att skapa en tillfällig fil är att använda in-memory lagringsmetoder som variabler, vilka kan vara snabbare men är begränsade till tillgängligt RAM.

När du använder [System.IO.Path]::GetTempFileName() för att skapa en tillfällig fil i PowerShell, skapas faktiskt filen i systemet under det anropet. Funktionen ger sedan tillbaka sökvägen till den nyskapade filen.

## Se Också:

* Microsoft dokumentation för [GetTempFileName()](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=net-5.0)
* Microsofts guide till [PowerShell](https://docs.microsoft.com/sv-se/powershell/scripting/overview?view=powershell-7.1)