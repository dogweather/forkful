---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:46.529419-07:00
description: "Een webpagina downloaden in Visual Basic for Applications (VBA) betekent\
  \ het ophalen van de HTML-inhoud van een webpagina van het internet. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:50.635454-06:00'
model: gpt-4-0125-preview
summary: "Een webpagina downloaden in Visual Basic for Applications (VBA) betekent\
  \ het ophalen van de HTML-inhoud van een webpagina van het internet. Programmeurs\u2026"
title: Een webpagina downloaden
---

{{< edit_this_page >}}

## Wat & Waarom?

Een webpagina downloaden in Visual Basic for Applications (VBA) betekent het ophalen van de HTML-inhoud van een webpagina van het internet. Programmeurs voeren deze taak vaak uit om de inhoud van websites programmatisch te verwerken of te analyseren, vanuit Excel, Access of andere Office-toepassingen.

## Hoe:

Om een webpagina in VBA te downloaden, kun je gebruikmaken van de Microsoft XML, v6.0 (MSXML6) bibliotheek, die server HTTP-verzoeken mogelijk maakt. Voordat je in de code duikt, zorg ervoor dat je deze referentie in je VBA-editor hebt ingeschakeld door naar `Extra` -> `Verwijzingen` te gaan en `Microsoft XML, v6.0` aan te vinken.

Hier is een eenvoudig voorbeeld van hoe je de HTML-inhoud van een webpagina kunt downloaden:

```basic
Sub DownloadWebpagina()
    Dim request As Object
    Dim url As String
    Dim response As String
    
    ' Initialiseren van het XML HTTP-verzoek object
    Set request = CreateObject("MSXML2.XMLHTTP")
    
    url = "http://www.voorbeeld.com"
    
    ' Een synchroon verzoek openen
    request.Open "GET", url, False
    
    ' Het verzoek naar de server sturen
    request.send
    
    ' De respons tekst verkrijgen
    response = request.responseText
    
    ' De respons naar het onmiddellijke venster uitvoeren (voor debugdoeleinden)
    Debug.Print response
    
    ' Opruimen
    Set request = Nothing
End Sub
```

Het uitvoeren van deze subroutine zal de HTML van `http://www.voorbeeld.com` afdrukken naar het Direct-venster in de VBA-editor. Let op dat de `False` parameter in de `Open` methode het verzoek synchroon maakt, wat betekent dat de code wacht tot de webpagina is gedownload voordat hij doorgaat met de volgende regel.

## Diepgaand

De getoonde techniek is gebaseerd op MSXML, Microsofts implementatie van de XML HTTP Request-standaard, vaak gebruikt voor AJAX-verzoeken in webontwikkeling. Dit onderdeel maakt al lange tijd deel uit van Microsofts technologiestapel, waardoor het een robuuste keuze is voor netwerkverzoeken in VBA.

Echter, afhankelijkheid van MSXML en VBA voor het downloaden en parsen van webinhoud kan beperkend zijn, met name bij moderne webapplicaties die veelvuldig gebruikmaken van JavaScript voor dynamische inhoudsweergave. Deze beperkingen kunnen andere talen of hulpmiddelen zoals Python met bibliotheken zoals BeautifulSoup of Selenium geschikter maken voor webscraping-taken vanwege hun vermogen om JavaScript uit te voeren en complexe website-interacties te hanteren.

Desondanks blijft VBA voor eenvoudige taken die het ophalen van rechttoe rechtaan HTML-inhoud betreffen of wanneer men binnen de grenzen van Office-applicaties werkt, een praktisch hulpmiddel. De integratie binnen de Office-suite biedt de mogelijkheid voor directe manipulatie van documenten op basis van webinhoud, wat een uniek voordeel biedt voor specifieke gebruikssituaties.
