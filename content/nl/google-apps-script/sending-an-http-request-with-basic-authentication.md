---
title:                "Een HTTP-verzoek verzenden met basisauthenticatie"
aliases:
- nl/google-apps-script/sending-an-http-request-with-basic-authentication.md
date:                  2024-02-01T22:02:49.045284-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden met basisauthenticatie"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/google-apps-script/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verzenden van een HTTP-verzoek met basisauthenticatie omvat het coderen van een gebruikersnaam en wachtwoord in een aanvraagheader om toegang te krijgen tot beschermde bronnen. Programmeurs gebruiken deze methode voor authenticatie aan de serverzijde, om te integreren met API's die basisauthenticatie vereisen voor operaties zoals het ophalen van gegevens of het plaatsen van inhoud.

## Hoe te:

In Google Apps Script stuur je een HTTP-verzoek met basisauthenticatie door gebruik te maken van de `UrlFetchApp`-service in combinatie met een base64-gecodeerde autorisatieheader. Hier is een stap-voor-stap handleiding:

1. **Coderen van inloggegevens**: Codeer eerst uw gebruikersnaam en wachtwoord in base64. Google Apps Script heeft geen native base64-coderingsfunctie voor strings, dus u gebruikt `Utilities.base64Encode` voor dit doel.

```javascript
var gebruikersnaam = 'UwGebruikersnaam';
var wachtwoord = 'UwWachtwoord';
var gecodeerdeInloggegevens = Utilities.base64Encode(gebruikersnaam + ':' + wachtwoord);
```

2. **Instellen van aanvraagopties**: Met de gecodeerde inloggegevens gereed, bereidt u het optieobject voor de HTTP-aanvraag voor, inclusief de methode en headers.

```javascript
var opties = {
  method: 'get', // of 'post', 'put', afhankelijk van uw behoeften
  headers: {
    'Authorization': 'Basic ' + gecodeerdeInloggegevens
  }
  // extra opties zoals 'muteHttpExceptions' voor foutafhandeling kunnen hier worden toegevoegd
};
```

3. **De aanvraag doen**: Gebruik de `UrlFetchApp.fetch`-methode met de doel-URL en het optieobject.

```javascript
var url = 'https://voorbeeld.com/api/bron';
var respons = UrlFetchApp.fetch(url, opties);
Logger.log(respons.getContentText());
```

Voorbeelduitvoer bij succesvolle aanvraag varieert op basis van de respons van de API. Voor een JSON-gebaseerde API zou je zoiets kunnen zien als:

```
{"status":"Succes","data":"Resource data hier..."}
```

Zorg ervoor dat u mogelijke HTTP-fouten afhandelt door de responscode te controleren of de optie `muteHttpExceptions` te gebruiken voor een meer gecontroleerde foutbeheer.

## Diepere duik

Het versturen van een HTTP-verzoek met basisauthenticatie is een standaardmethode geweest in veel programmeertalen voor het verkrijgen van toegang tot webgebaseerde bronnen die authenticatie vereisen. In de context van Google Apps Script biedt `UrlFetchApp` een eenvoudige manier om deze HTTP-verzoeken uit te voeren, inclusief die waarvoor authenticatie nodig is. Het opnemen van basisgegevens in de aanvraagheaders is een eenvoudige maar effectieve methode, maar brengt beveiligingsrisico's met zich mee, vooral omdat de inloggegevens in platte tekst worden verzonden, enkel base64-gecodeerd, wat gemakkelijk gedecodeerd kan worden als het onderschept wordt.

Voor verbeterde beveiliging worden alternatieven zoals OAuth 2.0 aanbevolen, vooral bij het omgaan met gevoelige gegevens of operaties. Google Apps Script heeft ingebouwde ondersteuning voor OAuth 2.0 met de `OAuth2`-bibliotheek, waardoor het proces van authenticatie tegen diensten die dit protocol ondersteunen, wordt gestroomlijnd.

Ondanks de beveiligingsbeperkingen blijft basisauthenticatie veel gebruikt voor eenvoudige of interne toepassingen die niet aan het bredere internet worden blootgesteld. Het is eenvoudig te implementeren, omdat het slechts één verzoek met goed ingestelde headers vereist, waardoor het een aantrekkelijke optie is voor snelle integraties of voor API's waar hogere beveiligingsmethoden niet beschikbaar zijn. Programmeurs worden echter aangespoord om de beveiligingsimplicaties te overwegen en veiligere alternatieven te verkennen wanneer beschikbaar.
