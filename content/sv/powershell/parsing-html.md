---
title:                "Tolka HTML"
date:                  2024-01-20T15:33:11.110045-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Parsing av HTML innebär att man analyserar HTML-kod för att extrahera specifik data — som att hitta text inuti vissa taggar. Programmerare gör det för att automatisera datainsamling från webbsidor och bearbeta innehållet.

## Hur gör man:
För att köra kodexemplen, se till att du har `Invoke-WebRequest` och `HtmlAgilityPack` tillgängliga i din PowerShell-session. `HtmlAgilityPack` är en .NET-bibliotek som gör det enklare att hantera HTML.

```PowerShell
# Installera HtmlAgilityPack
Install-Package HtmlAgilityPack -Scope CurrentUser

# Anropa en webbsida
$response = Invoke-WebRequest -Uri 'https://example.com'

# Ladda HTML in i ett HtmlDocument-objekt med HtmlAgilityPack
$html = New-Object HtmlAgilityPack.HtmlDocument
$html.LoadHtml($response.Content)

# Hitta alla element med en specifik klass
$nodes = $html.DocumentNode.SelectNodes("//div[@class='min-klass']")

# Skriv ut texten för varje node
$nodes | ForEach-Object { $_.InnerText }
```
Exempelutmatning:

```
Detta är den första texten i klassen 'min-klass'.
Här är lite mer text i en annan element med samma klass.
```

## Fördjupning
Att parse HTML är inte något nytt. Sedan webben blev mainstream på 90-talet har behovet av att bearbeta HTML-data vuxit. Tillbaka i tiden använde vi enklare regex-metoder, men de är ökända för att vara opålitliga för komplex HTML. HtmlAgilityPack är ett bättre verktyg för .NET och PowerShell, som hanterar HTML effektivt och på ett strukturerat sätt.

Alternativen till HtmlAgilityPack inkluderar andra bibliotek som AngleSharp för C# eller cheerio för JavaScript. De har liknande funktioner men skiljer sig åt i syntax och integration.

När du parse HTML, tänk på att webbsidors struktur kan ändras. Din kod kan behöva uppdateras om målwebbplatsen gör ändringar i sin HTML.

## Se även:
- [HtmlAgilityPack på GitHub](https://github.com/zzzprojects/html-agility-pack)
- [PowerShell Documentation - Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [AngleSharp GitHub](https://github.com/AngleSharp/AngleSharp)
- [cheerio GitHub](https://github.com/cheeriojs/cheerio)
