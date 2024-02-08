---
title:                "Att Tolka HTML"
aliases:
- sv/vba/parsing-html.md
date:                  2024-02-01T21:57:00.780914-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att Tolka HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/vba/parsing-html.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka (parse:a) HTML i Visual Basic for Applications (VBA) innebär att extrahera specifik information från ett HTML-dokument. Programmerare gör det för att automatisera processen att läsa och hantera data från webbsidor, som till exempel att skrapa webbplatsinnehåll eller automatisera formulärinlämningar och datahämtning, inom applikationer som Microsoft Excel eller Access som stöder VBA.

## Hur man gör:

I VBA kan du tolka HTML med hjälp av `Microsoft HTML Object Library`. Lägg till en referens till detta bibliotek i din VBA-editor genom att gå till Verktyg > Referenser och kryssa i `Microsoft HTML Object Library`. Detta ger dig tillgång till klasser för att navigera och manipulera HTML-dokument.

Här är ett enkelt exempel som visar hur man laddar ett HTML-dokument från en fil och extraherar alla länkar (ankartaggar):

```vb
Sub ParseHTML()
    Dim htmlDoc As MSHTML.HTMLDocument
    Dim htmlElement As MSHTML.IHTMLElement
    Dim htmlElements As MSHTML.IHTMLElementCollection
    Dim htmlFile As String
    Dim fileContent As String
    
    ' Ladda HTML-innehållet från en fil
    htmlFile = "C:\sökväg\till\din\fil.html"
    Open htmlFile For Input As #1
    fileContent = Input$(LOF(1), 1)
    Close #1
    
    ' Initiera HTML-dokument
    Set htmlDoc = New MSHTML.HTMLDocument
    htmlDoc.body.innerHTML = fileContent
    
    ' Hämta alla ankartaggar
    Set htmlElements = htmlDoc.getElementsByTagName("a")

    ' Loopa igenom alla ankar-element och skriv ut href-attributet
    For Each htmlElement In htmlElements
        Debug.Print htmlElement.getAttribute("href")
    Next htmlElement
End Sub
```

Detta skript läser innehållet i en HTML-fil, laddar det i ett `HTMLDocument`-objekt, hämtar alla ankarelement (`<a>`-taggar) och itererar sedan över dem, och skriver ut `href`-attributet för varje till det omedelbara fönstret.

## Fördjupning:

Historiskt sett har tolkning av HTML i VBA varit lite besvärlig på grund av bristen på direkt stöd för moderna teknologier för webbskrapning och hantering av dokument. Microsoft HTML Object Library, trots att det är kraftfullt, är något daterat och kanske inte hanterar moderna webbstandarder så smidigt som nyare teknologier.

För komplexa HTML-tolkningar och webbskrapningsuppgifter rekommenderas ofta alternativa verktyg och språk som Python med bibliotek såsom Beautiful Soup eller Scrapy. Dessa moderna verktyg erbjuder mer flexibilitet, bättre prestanda och är mer i linje med nuvarande webbstandarder. Dock, när man arbetar inom Microsoft Office-ekosystemet, är användningen av VBA med Microsoft HTML Object Library fortfarande en värdefull färdighet. Det låser upp direkt manipulation av HTML-innehåll på ett sätt som integreras sömlöst med applikationer som Excel och Access, och ger en enkel metod för att utföra uppgifter som involverar grundläggande hantering av HTML-dokument utan att behöva kliva utanför den bekanta VBA-miljön.
