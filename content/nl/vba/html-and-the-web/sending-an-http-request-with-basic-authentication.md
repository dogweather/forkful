---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:26.826001-07:00
description: "Het versturen van een HTTP-verzoek met basisauthenticatie in Visual\
  \ Basic for Applications (VBA) gaat over het toegang krijgen tot webbronnen die\
  \ zijn\u2026"
lastmod: '2024-03-13T22:44:50.636421-06:00'
model: gpt-4-0125-preview
summary: "Het versturen van een HTTP-verzoek met basisauthenticatie in Visual Basic\
  \ for Applications (VBA) gaat over het toegang krijgen tot webbronnen die zijn\u2026"
title: Een HTTP-verzoek verzenden met basisauthenticatie
weight: 45
---

## Wat & Waarom?

Het versturen van een HTTP-verzoek met basisauthenticatie in Visual Basic for Applications (VBA) gaat over het toegang krijgen tot webbronnen die zijn beveiligd met gebruikersnaam- en wachtwoordgegevens. Programmeurs doen dit om te interageren met beveiligde API's of webservice binnen hun VBA-gedreven applicaties, zoals het automatiseren van taken in Excel of Access met gegevens van beveiligde eindpunten.

## Hoe:

In VBA kunt u de `Microsoft XML, v6.0` (MSXML2) bibliotheek gebruiken om HTTP-verzoeken met basisauthenticatie te verzenden. Dit houdt in dat u de `"Authorization"` header van het verzoek instelt om de referenties in een base64-gecodeerd formaat op te nemen. Hier is een stap-voor-stap handleiding:

1. **Verwijs naar MSXML2**: Zorg eerst dat uw VBA-project verwijst naar de `Microsoft XML, v6.0` bibliotheek. Ga in de VBA-editor naar Extra > Verwijzingen en vink `Microsoft XML, v6.0` aan.

2. **Maak en verzend het HTTP-verzoek**: Gebruik het volgende VBA-codestuk als een gids. Vervang `"your_username"` en `"your_password"` door uw daadwerkelijke referenties en pas de URL naar behoefte aan.

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' Vervang door de daadwerkelijke URL
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' Print de reactie uit naar het Directe Venster
    ```

3. **Codeer referenties in base64**: VBA heeft geen ingebouwde functie voor base64-codering, maar u kunt deze aangepaste `EncodeBase64` functie gebruiken:

    ```vb
    Function EncodeBase64(text As String) As String
        Dim arrData() As Byte
        arrData = StrConv(text, vbFromUnicode)
        
        Dim objXML As MSXML2.DOMDocument60
        Dim objNode As MSXML2.IXMLDOMElement
        
        Set objXML = New MSXML2.DOMDocument60
        Set objNode = objXML.createElement("b64")
        
        objNode.dataType = "bin.base64"
        objNode.nodeTypedValue = arrData
        EncodeBase64 = objNode.Text
    End Function
    ```
    
Dit zal een GET-verzoek naar `http://example.com/api/resource` verzenden met de opgegeven basisauthenticatie referenties, en de respons uitprinten.

## Diepere Duik

De hier gebruikte benadering, hoewel effectief voor eenvoudige gebruikssituaties, is gebaseerd op het Basic Authentication schema, dat referenties verzendt in een gemakkelijk decodeerbaar formaat (base64-codering is geen encryptie). Vanwege de kwetsbaarheid, vooral in niet-HTTPS-contexten, wordt basisauthenticatie niet aanbevolen voor het overbrengen van gevoelige gegevens over het internet zonder aanvullende beveiligingslagen zoals SSL/TLS.

Historisch gezien was Basic Authentication een van de eerste methoden ontwikkeld voor het beheersen van toegang tot webbronnen. Vandaag de dag worden veiligere en flexibelere authenticatienormen, zoals OAuth 2.0, over het algemeen de voorkeur gegeven voor nieuwe toepassingen. Gezien de beperkingen van VBA en de externe afhankelijkheden die vereist zijn voor geavanceerdere authenticatiemethoden, gebruiken ontwikkelaars VBA vaak in interne of minder beveiligingskritieke omgevingen of gebruiken het als een springplank om snel ideeën te prototypen.

Wanneer u VBA gebruikt voor HTTP-verzoeken, onthoud dan dat elke versie van de MSXML-bibliotheek verschillende kenmerken en beveiligingsstandaarden kan ondersteunen. Gebruik altijd de meest recente versie die compatibel is met uw applicatie om betere beveiliging en prestaties te garanderen. Overweeg ook de milieu-beperkingen en mogelijk verouderde functies bij het kiezen van VBA voor nieuwe projecten, met name diegene die veilige HTTP-communicaties vereisen. Andere programmeeromgevingen of talen kunnen robuustere, veiligere en onderhoudbare oplossingen bieden voor vergelijkbare taken.
