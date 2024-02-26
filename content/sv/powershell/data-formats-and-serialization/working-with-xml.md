---
date: 2024-01-26 04:34:20.739203-07:00
description: "Att arbeta med XML inneb\xE4r att manipulera och komma \xE5t data strukturerad\
  \ i det utvidgbara m\xE4rkspr\xE5ket. Programmerare arbetar med XML f\xF6r att m\xF6\
  jligg\xF6ra\u2026"
lastmod: '2024-02-25T18:49:36.465378-07:00'
model: gpt-4-0125-preview
summary: "Att arbeta med XML inneb\xE4r att manipulera och komma \xE5t data strukturerad\
  \ i det utvidgbara m\xE4rkspr\xE5ket. Programmerare arbetar med XML f\xF6r att m\xF6\
  jligg\xF6ra\u2026"
title: Att arbeta med XML
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med XML innebär att manipulera och komma åt data strukturerad i det utvidgbara märkspråket. Programmerare arbetar med XML för att möjliggöra interoperabilitet med andra system eller för att läsa och skriva konfigurationsfiler, datakällor och andra strukturerade dokument som är vanliga i webbtjänster.

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
