---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:54.381527-07:00
description: "Hur man g\xF6r: F\xF6r att b\xF6rja interagera med XML anv\xE4nder man\
  \ vanligtvis objektet `MSXML2.DOMDocument`. Det h\xE4r gr\xE4nssnittet g\xF6r det\
  \ m\xF6jligt f\xF6r dig att\u2026"
lastmod: '2024-03-13T22:44:37.769910-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att b\xF6rja interagera med XML anv\xE4nder man vanligtvis objektet\
  \ `MSXML2.DOMDocument`."
title: Arbeta med XML
weight: 40
---

## Hur man gör:
För att börja interagera med XML använder man vanligtvis objektet `MSXML2.DOMDocument`. Det här gränssnittet gör det möjligt för dig att ladda, analysera och navigera XML-dokument. Nedan är ett enkelt exempel som visar hur man laddar en XML-fil, navigerar dess struktur och läser attribut och textinnehåll.

```basic
' Först, se till att du har lagt till referensen till "Microsoft XML, v6.0" via Verktyg -> Referenser
Dim xmlDoc As MSXML2.DOMDocument60
Set xmlDoc = New MSXML2.DOMDocument60
xmlDoc.async = False
xmlDoc.Load("C:\Sökväg\Till\Din\Fil.xml") ' Ladda din XML-fil

' Kontrollera om XML laddades framgångsrikt
If xmlDoc.parseError.ErrorCode <> 0 Then
    MsgBox "Fel vid laddning av XML:" & xmlDoc.parseError.reason
Else
    ' Navigera och läs element
    Dim book As IXMLDOMNode
    Set book = xmlDoc.SelectSingleNode("//book/title") ' XPath för att hitta den första <title> inuti <book>
    MsgBox book.Text ' Visa titeltexten
End If
```

I exemplet ovan skapar vi en instans av `MSXML2.DOMDocument60`, laddar en XML-fil och kontrollerar sedan efter fel. Om inga fel hittas navigerar vi till en specifik nod med hjälp av XPath och visar dess textinnehåll.

## Fördjupning:
Integrationen av XML-funktioner i VBA går tillbaka till början av 2000-talet när behovet av att Office-applikationer skulle interagera med webbdata och tjänster började växa. `MSXML`-biblioteket, eller Microsoft XML Core Services, har utvecklats över åren, med `MSXML2.DOMDocument60` som en av de senaste versionerna som rekommenderas för användning på grund av dess förbättrade prestanda och säkerhetsfunktioner.

Även om kraftfulla, anses XML-hanteringsegenskaperna hos VBA vara mindre effektiva och mer omständliga jämfört med moderna programmeringsmiljöer som Python's XML.etree eller C#'s LINQ till XML. Den inneboende verbositeten hos VBA och kravet på att manuellt lägga till och hantera referenser kan avskräcka från snabb utveckling. Vidare, med framkomsten av JSON som ett mer lättviktigt datautbytesformat, flyttar många programmerare och applikationer bort från XML om inte interoperabilitet med äldre system eller specifika företagstjänster kräver dess användning.

Dock, för uppgifter som kräver analys eller generering av XML-dokument inom ramen för Microsoft Office-automatisering, är det att utnyttja VBAs XML-hanteringsfunktioner fortfarande ett gångbart och ibland nödvändigt tillvägagångssätt. Detta skapar en balans mellan att komma åt det rika funktionssättet hos Office-applikationer och de strukturerade datahanteringsmöjligheterna som XML erbjuder.
