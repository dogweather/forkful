---
title:                "Een HTTP-verzoek verzenden"
aliases: - /nl/vba/sending-an-http-request.md
date:                  2024-02-01T22:02:03.289938-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/vba/sending-an-http-request.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een HTTP-verzoek verzenden in Visual Basic for Applications (VBA) houdt in dat je programmatisch toegang krijgt tot webbronnen of webservices door verzoeken over HTTP te doen. Programmeurs doen dit om gegevens op te halen, interactie aan te gaan met online API's of formulieren programmatisch in te dienen vanuit hun VBA-geschikte applicaties zoals Excel, Access of op maat gemaakte VBA-oplossingen.

## Hoe te:

De sleutel tot het verzenden van een HTTP-verzoek in VBA is het gebruik van de `Microsoft XML, v6.0` bibliotheek (of oudere versies, afhankelijk van je systeem). Zorg eerst dat deze referentie is ingeschakeld in je project door naar Extra > Referenties te gaan in de VBA-editor en `Microsoft XML, v6.0` aan te vinken.

Hier is hoe je een eenvoudig HTTP GET-verzoek verstuurt:

```vb
Dim httpRequest As Object
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")

Met httpRequest
    .Open "GET", "https://api.example.com/data", False
    .send
    Als .Status = 200 Dan
        Debug.Print .responseText
    Anders
        Debug.Print "Fout: " & .Status & " - " & .statusText
    Einde Als
Einde Met
```

Voor een POST-verzoek, waar we gegevens (bijv. JSON) naar een server moeten versturen:

```vb
Dim httpRequest Als Object, postData Als String
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")
postData = "{""sleutel"":""waarde""}"

Met httpRequest
    .Open "POST", "https://api.example.com/indienen", False
    .setRequestHeader "Content-Type", "application/json"
    .send postData
    Als .Status = 200 Dan
        Debug.Print .responseText
    Anders
        Debug.Print "Fout: " & .Status & " - " & .statusText
    Einde Als
Einde Met
```

Voorbeelduitvoer voor een succesvol verzoek kan een JSON-string of een HTML-pagina zijn, afhankelijk van de API of webpagina waarmee je interactie hebt:

```
{"data": "Dit is de respons van de server"}
```

## Verdieping

De getoonde methode gebruikt het `MSXML2.XMLHTTP` object, onderdeel van de Microsoft XML Core Services (MSXML). Het is geïntroduceerd om VBA-ontwikkelaars een manier te bieden om XML-gebaseerde bewerkingen uit te voeren en werd in de loop van de tijd een algemeen hulpmiddel voor HTTP-verzoeken, zelfs wanneer niet direct met XML-gegevens wordt gewerkt. Ondanks de leeftijd blijft het een betrouwbare optie voor eenvoudige webinteracties in VBA.

Echter, VBA en zijn HTTP-verzoekmechanismen missen de robuustheid en flexibiliteit die te vinden zijn in hedendaagse programmeeromgevingen. Bijvoorbeeld, het afhandelen van asynchrone verzoeken of het werken binnen applicaties die geavanceerde HTTP-functies vereisen (zoals websockets of server-sent events) valt buiten het bereik van VBA. Bij het werken aan complexere webintegratieprojecten doen ontwikkelaars vaak een beroep op externe bibliotheken of hulpmiddelen, of zelfs op het automatiseren van browsergedrag via webschraaptechnieken, hoewel deze meer werkaround dan oplossingen zijn.

Talen en omgevingen zoals Python met zijn `requests` bibliotheek of JavaScript draaiend op Node.js bieden krachtigere en veelzijdigere HTTP-verzoekmogelijkheden direct uit de doos, inclusief asynchrone operaties, eenvoudigere JSON-afhandeling en uitgebreide ondersteuning voor verschillende webtechnologieën. Ontwikkelaars die diep geworteld zijn in het Microsoft-ecosysteem, kunnen overwegen over te stappen op PowerShell of C# voor taken die een meer geavanceerde webinteractie vereisen, door gebruik te maken van .NET's uitgebreide netwerkprogrammeerfuncties.

Zodoende, terwijl VBA's mogelijkheden voor HTTP-verzoeken voldoende zijn voor eenvoudige query's en gegevensophaaltaken, wordt het verkennen van alternatieven cruciaal naarmate de eisen van je project evolueren naar het complexe en moderne weblandschap.
