---
title:                "Sända en http-begäran"
html_title:           "PowerShell: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-begäran är en viktig del av programmering. Det betyder helt enkelt att skicka ett meddelande till en webbserver och begära en resurs. Detta är nödvändigt för att hämta och skicka data mellan olika webbplatser eller applikationer. 

## Så här gör du:

Med PowerShell är det enkelt att skicka en HTTP-begäran. Allt du behöver är cmdleten "Invoke-RestMethod" och en URL som pekar på den webbserver du vill hämta data från. Här är ett enkelt exempel: 

```
$url = "https://www.example.com/api/users"
$users = Invoke-RestMethod -Uri $url
$users | Select-Object -Property name, email
```
Det här kommandot kommer att hämta en lista över användare från webbservern och visa deras namn och e-postadresser. Du kan även skicka data till en webbserver genom att använda parametern "-Body" med "Invoke-RestMethod". 

## Djupdykning:

Historiskt sett var det vanligt att använda andra språk eller verktyg för att skicka HTTP-begäran, som till exempel cURL eller wget. Men med PowerShell har vi nu en enkel och lättanvändlig metod som är inbyggd i systemet. 

Det finns också andra sätt att skicka HTTP-begäran på, till exempel med cmdleten "Invoke-WebRequest" eller genom att använda .NET-ramverket. Men "Invoke-RestMethod" är det rekommenderade sättet att skicka och hämta data från webbserverar. 

När du skickar en HTTP-begäran är det viktigt att vara medveten om vilken metod som används, till exempel GET eller POST, och att rätt URL används för att få det önskade svaret. Du kan också ange headers och andra parametrar för att anpassa din begäran ännu mer. Se PowerShell-dokumentationen för mer information och exemplen. 

## Se även:

- [PowerShell dokumentetation för Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7)
- [Guide för att skicka HTTP-begäran med PowerShell](https://adamtheautomator.com/powershell-http-methods/)
- [Mer detaljerad guid för att skicka HTTP-begäran med PowerShell](https://devops-collective-inc.gitbook.io/psql-challenge/psql-skicka-din-forsta-http-begaran-med-powershell)