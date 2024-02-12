---
title:                "Sende en HTTP-forespørsel"
date:                  2024-02-01T22:01:46.274847-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sende en HTTP-forespørsel"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/google-apps-script/sending-an-http-request.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Å sende en HTTP-forespørsel i Google Apps Script handler om å programmert utføre et kall til en ekstern webserver eller API. Programmerere gjør dette for å hente eller sende data til webtjenester, og integrerer et bredt spekter av webressurser og funksjonaliteter direkte inn i sine Google Apps Script-prosjekter.

## Hvordan:

I Google Apps Script er den primære måten å sende en HTTP-forespørsel på, ved å bruke `UrlFetchApp`-tjenesten. Denne tjenesten gir metoder for å gjøre HTTP GET- og POST-forespørsler. Her er et enkelt eksempel på å gjøre en GET-forespørsel for å hente JSON-data:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

For en POST-forespørsel, som ofte brukes til å sende data til en server, må du inkludere flere detaljer i alternativparameteret:

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    key1: 'verdi1',
    key2: 'verdi2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // Konverter JavaScript-objektet til en JSON-streng
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

Disse kodestykkene viser grunnleggende implementeringer av GET- og POST-forespørsler. Utdataene vil avhenge av API-responsen og kan sees i Google Apps Scripts Logger.

## Fordypning

Google Apps Scripts `UrlFetchApp`-tjeneste har utviklet seg betydelig siden starten, og tilbyr mer nyansert kontroll over HTTP-forespørsler med funksjoner som å sette overskrifter, last og håndtere multipart/form-data for filopplastinger. Selv om den gir et enkelt middel for å integrere eksterne webtjenester, kan utviklere som kommer fra mer robuste bakendspråk finne funksjonaliteten noe begrensende sammenlignet med biblioteker som Pythons `requests` eller JavaScripts `fetch` API i Node.js.

En bemerkelsesverdig begrensning er utførelsestidsgrensen for Google Apps Script, som påvirker langvarige forespørsler. I tillegg, selv om `UrlFetchApp` dekker et bredt spekter av bruksområder, kan mer komplekse scenarioer som involverer OAuth-autentisering eller håndtering av veldig store nyttelaster kreve kreative løsninger eller benytte ytterligere Google Cloud-ressurser.

Likevel, for de fleste integreringer som Google Workspace-utviklere støter på—fra automatisering av datahenting til å poste oppdateringer til eksterne tjenester—gir `UrlFetchApp` et kraftig, tilgjengelig verktøy. Dets integrering i Google Apps Script betyr at det ikke er behov for eksterne biblioteker eller kompleks oppsett, noe som gjør HTTP-forespørsler relativt enkle å utføre innenfor rammene av Google Apps Script. Ettersom landskapet av web-APIer fortsetter å ekspandere, forblir `UrlFetchApp` en kritisk bro for Google Apps Script-programmer til å samhandle med verdenen utenfor Googles økosystem.