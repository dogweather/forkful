---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:53.019390-07:00
description: "JSON, eller JavaScript Object Notation, \xE4r ett l\xE4ttviktigt format\
  \ f\xF6r lagring och transport av data, idealiskt f\xF6r kommunikation server-till-klient\
  \ samt\u2026"
lastmod: '2024-03-13T22:44:37.462215-06:00'
model: gpt-4-0125-preview
summary: "JSON, eller JavaScript Object Notation, \xE4r ett l\xE4ttviktigt format\
  \ f\xF6r lagring och transport av data, idealiskt f\xF6r kommunikation server-till-klient\
  \ samt\u2026"
title: Arbeta med JSON
---

{{< edit_this_page >}}

## Vad & Varför?

JSON, eller JavaScript Object Notation, är ett lättviktigt format för lagring och transport av data, idealiskt för kommunikation server-till-klient samt konfigurationsfiler. Programmerare nyttjar det i Google Apps Script för sömlöst datautbyte mellan Googletjänster (som Sheets, Docs, Drive) och externa källor, tack vare dess lättlästa struktur och enkla integration i JavaScript-baserade miljöer.

## Hur man gör:

I Google Apps Script är manipulering av JSON en enkel process, mycket tack vare det inhemska stödet JavaScript erbjuder för tolkning och omvandling av JSON. Här är några vanliga operationer:

**1. Tolka JSON**: Antag att vi hämtar en JSON-sträng från en webbtjänst; att tolka den till ett JavaScript-objekt är avgörande för datamanipulation.

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // Utdata: Sample Project
```

**2. Omvandla JavaScript-Objekt till JSON-strängar**: Tvärtom, att omvandla ett JavaScript-objekt till en JSON-sträng är användbart när vi behöver skicka data från Apps Script till en extern tjänst.

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // Utdata: '{"name":"Sample Project","version":"1.0.0"}'
```

**3. Arbeta med komplex data**:
För mer komplexa datastrukturer, som arrayer av objekt, förblir processen densamma, vilket visar på JSON:s flexibilitet för datåtergivning.

```javascript
var projects = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // Utdata: '[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## Fördjupning

JSON:s allestädes närvaro i moderna webbapplikationer kan inte underdrivas, rotad i dess enkelhet och hur sömlöst det integreras med JavaScript, webbens språk. Dess design, inspirerad av JavaScript-objektlitteraler, om än striktare, underlättar dess snabba anammande. I början av 2000-talet vann JSON popularitet som ett alternativ till XML för AJAX-drivna webbapplikationer, och erbjuder ett lättviktigare och mindre verbose datautbytesformat. Med tanke på Google Apps Scripts djupa integration med olika Google API:er och externa tjänster, tjänar JSON som ett centralt format för strukturering, transport och manipulation av data över dessa plattformar.

Medan JSON regerar överlägset för webbapplikationer, finns alternativa dataformat som YAML för konfigurationsfiler eller Protobuf för effektivare binär serialisering i högpresterande miljöer. Dock cementerar JSON:s balans av läslighet, användarvänlighet och breda stöd över programmeringsspråk och verktyg dess position som ett förstahandsval för många utvecklare som ger sig in i Google Apps Script och vidare.
