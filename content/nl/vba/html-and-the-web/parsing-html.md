---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:46.344913-07:00
description: "HTML parsen in Visual Basic for Applications (VBA) houdt in dat specifieke\
  \ informatie uit een HTML-document wordt ge\xEBxtraheerd. Programmeurs doen dit\
  \ om\u2026"
lastmod: 2024-02-19 22:05:09.684542
model: gpt-4-0125-preview
summary: "HTML parsen in Visual Basic for Applications (VBA) houdt in dat specifieke\
  \ informatie uit een HTML-document wordt ge\xEBxtraheerd. Programmeurs doen dit\
  \ om\u2026"
title: HTML Parsen
---

{{< edit_this_page >}}

## Wat & Waarom?

HTML parsen in Visual Basic for Applications (VBA) houdt in dat specifieke informatie uit een HTML-document wordt geëxtraheerd. Programmeurs doen dit om het proces van het lezen en verwerken van gegevens van webpagina's te automatiseren, zoals het schrapen van website-inhoud of het automatiseren van formuliersubmissies en gegevensopvragingen, binnen applicaties zoals Microsoft Excel of Access die VBA ondersteunen.

## Hoe:

In VBA kun je HTML parsen met behulp van de `Microsoft HTML Object Library`. Voeg een referentie naar deze bibliotheek toe in je VBA-editor door naar Extra > Referenties te gaan en `Microsoft HTML Object Library` aan te vinken. Dit geeft je toegang tot klassen voor het navigeren en manipuleren van HTML-documenten.

Hier is een eenvoudig voorbeeld dat laat zien hoe je een HTML-document vanuit een bestand laadt en alle links (ankertags) extrahereert:

```vb
Sub ParseHTML()
    Dim htmlDoc As MSHTML.HTMLDocument
    Dim htmlElement As MSHTML.IHTMLElement
    Dim htmlElements As MSHTML.IHTMLElementCollection
    Dim htmlFile As String
    Dim fileContent As String
    
    ' HTML-inhoud laden vanuit een bestand
    htmlFile = "C:\pad\naar\uw\bestand.html"
    Open htmlFile For Input As #1
    fileContent = Input$(LOF(1), 1)
    Close #1
    
    ' HTML Document initialiseren
    Set htmlDoc = New MSHTML.HTMLDocument
    htmlDoc.body.innerHTML = fileContent
    
    ' Alle ankertags verkrijgen
    Set htmlElements = htmlDoc.getElementsByTagName("a")

    ' Door alle ankerelementen lopen en de href-attribuut printen
    For Each htmlElement In htmlElements
        Debug.Print htmlElement.getAttribute("href")
    Next htmlElement
End Sub
```

Dit script leest de inhoud van een HTML-bestand, laadt dit in een `HTMLDocument`-object, haalt alle ankerelementen (`<a>` tags) op en loopt vervolgens over ze heen, waarbij het de `href`-attribuut van elk naar het Directe Venster print.

## Diepere Duik:

Historisch gezien was het parsen van HTML in VBA een beetje omslachtig vanwege het gebrek aan directe ondersteuning voor moderne technologieën voor webscraping en documentverwerking. De Microsoft HTML Object Library, hoewel krachtig, is enigszins verouderd en kan mogelijk niet zo soepel omgaan met moderne webstandaarden als nieuwere technologieën.

Voor complexe HTML-parsing- en webscraping-taken worden vaak alternatieve tools en talen zoals Python met bibliotheken zoals Beautiful Soup of Scrapy aanbevolen. Deze moderne tools bieden meer flexibiliteit, betere prestaties en zijn beter afgestemd op de huidige webstandaarden. Echter, bij het werken binnen het Microsoft Office-ecosysteem, blijft het gebruik van VBA met de Microsoft HTML Object Library een waardevolle vaardigheid. Het ontgrendelt directe manipulatie van HTML-inhoud op een manier die naadloos integreert met applicaties zoals Excel en Access, en biedt een eenvoudige methode voor het uitvoeren van taken die basis HTML-documentbehandeling omvatten zonder de vertrouwde VBA-omgeving te hoeven verlaten.
