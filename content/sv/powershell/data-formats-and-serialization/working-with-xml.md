---
date: 2024-01-26 04:34:20.739203-07:00
description: "Hur man g\xF6r: ."
lastmod: '2024-03-13T22:44:38.150354-06:00'
model: gpt-4-0125-preview
summary: .
title: Att arbeta med XML
weight: 40
---

## Hur man gör:
```PowerShell
# Ladda en XML-fil in i en variabel
[xml]$xmlContent = Get-Content 'sökväg\till\din\fil.xml'

# Komma åt XML-noder
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "Titel: $($book.title)"
}

# Skapa ett nytt XML-element
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# Spara XML tillbaka till fil
$xmlContent.Save('sökväg\till\din\uppdaterade\fil.xml')
```
Exempel på utdata:
```
Titel: Programmera PowerShell
Titel: XML Grundläggande
```

## Djupdykning
XML eller eXtensible Markup Language har funnits sedan slutet av 90-talet och är fortfarande ett brett använt format för strukturerade data. PowerShell förenklar arbetet med XML jämfört med traditionella tolkningsmetoder; det kastar XML till objekt direkt, vilket låter dig interagera med elementen genom bekant punktnotation.

Alternativ till XML inkluderar JSON, YAML eller egna dataformat. JSON har till exempel blivit populärt för sin lätta natur och enkel användning med webbteknologier. Dock gör XML:s utökade funktioner som namnrymder, scheman och XSLT-behandling det ofta till ett bättre val för komplexa dokument eller branschstandarder.

PowerShell använder .NET Frameworks XML-funktioner för sin XML-hantering. Detta innebär att det inte bara handlar om enkla läs-skrivoperationer; du kan också arbeta med XML-scheman för validering, använda XPath för förfrågningar och tillämpa XSLT-transformeringar, allt genom PowerShell.

## Se också
- [W3Schools XML-tutorial](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/json-en.html)
