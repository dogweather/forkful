---
aliases:
- /nl/javascript/sending-an-http-request-with-basic-authentication/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:07.396267-07:00
description: "Hier is een snel voorbeeld met behulp van JavaScript's Fetch API: ```javascript\
  \ const url = 'https://some-protected-resource.com/data'; const username =\u2026"
lastmod: 2024-02-18 23:09:02.275478
model: gpt-4-0125-preview
summary: "Hier is een snel voorbeeld met behulp van JavaScript's Fetch API: ```javascript\
  \ const url = 'https://some-protected-resource.com/data'; const username =\u2026"
title: Een HTTP-verzoek verzenden met basisauthenticatie
---

{{< edit_this_page >}}

# Wat & Waarom?

Een HTTP-verzoek verzenden met basisauthenticatie omvat het opnemen van een gebruikersnaam en wachtwoord gecodeerd in base64 binnen de verzoekheader. Programmeurs doen dit om toegang te krijgen tot bronnen die enige vorm van eenvoudige validatie behoeven om een zeker niveau van beveiliging te garanderen.

## Hoe:

Hier is een snel voorbeeld met behulp van JavaScript's Fetch API:

```javascript
const url = 'https://some-protected-resource.com/data';
const username = 'YourUsername';
const password = 'YourPassword';

const headers = new Headers();
headers.set('Authorization', 'Basic ' + btoa(username + ':' + password));

fetch(url, { method: 'GET', headers: headers })
  .then(response => {
    if (response.ok) return response.json();
    throw new Error('Network response was not ok.');
  })
  .then(data => console.log(data))
  .catch(error => console.error('Fetch-fout: ', error));
```

Voorbeelduitvoer (afgedrukt naar de console):

```javascript
{
  "protected": "data",
  "moreData": 12345
}
```

## Diepere Duik

Voordat we beginnen, laten we wat context krijgen. Basisauthenticatie is een van de eenvoudigste vormen van webservicesbeveiliging, die referenties verzendt in headers bij elk verzoek.

Historische Context:
- Basis HTTP-auth is een oude methode, oorspronkelijk uiteengezet in de RFC 7617 uit 2015, ter vervanging van de nog oudere RFC 2617 uit 1999.
- Het werd veel gebruikt vanwege zijn eenvoud, maar is niet zo veilig zonder HTTPS, aangezien base64 codering eenvoudig omkeerbaar is.

Alternatieven:
- OAuth: Een veiligere en complexere standaard voor toegangsdelegatie, gebruikt in gevallen waarin u toegang moet bieden zonder wachtwoordgegevens te delen.
- API-sleutels: Een enkele token die gemakkelijker te beheren is dan complexe OAuth-protocollen.
- Bearer Tokens: In het bijzonder JWT (JSON Web Tokens), die meer informatie kunnen dragen.

Implementatiedetails:
- Base64-codering transformeert de gebruikersnaam:wachtwoord-reeks in een reeks tekens die breder overdraagbaar is.
- Zorg altijd voor een HTTPS-verbinding, om te voorkomen dat inloggegevens worden onderschept.
- Moderne ontwikkeling geeft de voorkeur aan tokens en sessiecookies voor authenticatie, aangezien ze veiliger en veelzijdiger zijn.

## Zie Ook

- [Mozilla Developer Network - Autorisatie](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [RFC 7617 - HTTP Basis Auth](https://tools.ietf.org/html/rfc7617)
- [Introductie tot OAuth 2.0](https://www.digitalocean.com/community/tutorials/an-introduction-to-oauth-2)
- [JSON Web Tokens (JWT)](https://jwt.io/introduction/)
