---
title:                "Omvandling ett datum till en sträng"
html_title:           "PowerShell: Omvandling ett datum till en sträng"
simple_title:         "Omvandling ett datum till en sträng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera ett datum till en sträng är ett sätt för programmerare att omvandla ett datum i ett specifikt format till en textsträng. Det kan vara användbart för att visa eller spara datum i en läsbart format eller när man behöver skicka datumet till en extern plats som kräver textsträngar istället för datumobjekt.

## Så här gör du:
För att konvertera ett datum till en sträng i PowerShell använder du kommandot ```Get-Date -format "yyyy/MM/dd"``` där "yyyy/MM/dd" är det format som datumet ska visas i. Det finns flera olika format som du kan använda för att anpassa hur datumen visas, till exempel "M/d/yyyy" för månad/dag/år eller "hh:mm:ss tt" för timme:minut:sekund och AM/PM.

Ett annat användbart exempel är att kombinera datum och tidsinformation genom att använda variabeln ```$datetime = Get-Date``` som sparar datumet i en variabel och sedan använda ```$datetime.ToString("yyyy-MM-dd HH:mm:ss")``` för att få ut både datum och tid i formatet "år-månad-dag timme:minut:sekund".

## Djupdykning:
Konceptet att konvertera ett datum till en sträng är inte unikt för PowerShell utan finns även i andra programmeringsspråk. Det finns också flera olika sätt att konvertera datum till strängar, antingen genom att använda inbyggda funktioner eller skriva egen kod för att formatera datumen.

I vissa fall kan det vara mer lämpligt att använda datumobjekt istället för strängar i din kod, speciellt om du behöver göra mer avancerade beräkningar med datumen. Men att konvertera till strängar kan vara användbart vid presentation eller för att möta krav från externa system.

## Se även:
* [Get-Date PowerShell dokumentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date)
* [DateTime.ToString() C# dokumentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
* [Date and Time Formatting and Parsing in .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)