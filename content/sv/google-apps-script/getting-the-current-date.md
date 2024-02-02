---
title:                "Hämta aktuellt datum"
date:                  2024-02-01T21:54:48.584747-07:00
model:                 gpt-4-0125-preview
simple_title:         "Hämta aktuellt datum"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/google-apps-script/getting-the-current-date.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att få det aktuella datumet i Google Apps Script handlar om att hämta det levande datumet och tiden, en vanlig uppgift för att automatisera uppgifter, logga och tidsstämpla i appar knutna till Googles ekosystem. Programmerare använder detta för dynamisk innehållsgenerering, deadline-spårning och schemaläggning inom Google Docs, Sheets och andra Googletjänster.

## Hur man gör:

Google Apps Script, som bygger på JavaScript, erbjuder enkla metoder för att få det aktuella datumet. Du kan använda konstruktören `new Date()` för att skapa ett nytt datumobjekt som representerar det aktuella datumet och tiden. Så här kan du manipulera och visa detta i olika format.

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // Loggar det aktuella datumet och tiden i skriptets tidszon
  
  // För att bara visa datumet i YYYY-MM-DD-format
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // Exempelutskrift: "2023-04-01"
  
  // Visar i ett lättare läsbart format
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // Exempelutskrift: "April 1, 2023, 12:00:00 PM GMT+1"
}
```

Dessa kodsnuttar demonstrerar hur man fångar och formaterar det aktuella datumet och tiden, och visar på mångsidigheten för olika programmeringsbehov inom Google Apps Script.

## Fördjupning

Innan JavaScript fastställde `Date`-objektet, var programmerare tvungna att manuellt hålla koll på tid och datum genom mindre standardiserade och mer otympliga medel. Detta inkluderade användning av tidsstämpelintegers och hemmagjorda datumfunktioner, vilka varierade från en programmeringsmiljö till en annan och ledde till inkonsekvens och kompatibilitetsproblem.

Introduktionen av `new Date()`-objektet i JavaScript, och därigenom Google Apps Script, standardiserade datum- och tidsoperationer, vilket gjorde dem mer intuitiva och minskade mängden kod som behövs för datumrelaterade operationer. Det är värt att notera att även om Googles Apps Scripts implementering är bekväm och tillräcklig för många tillämpningar inom Googles produktserie, kan den inte tillgodose alla scenarier, särskilt de som kräver komplex tidszonshantering eller exakt tidstämpelloggning i snabba miljöer.

För sådana avancerade användningsfall vänder sig programmerare ofta till bibliotek som Moment.js eller date-fns i JavaScript. Även om Google Apps Script inte stöder dessa bibliotek inbyggt, kan utvecklare efterlikna några av deras funktioner med tillgängliga JavaScript Date-metoder eller genom att komma åt externa bibliotek genom HTML-tjänsten eller Apps Scripts URL Fetch-tjänst. Trots dessa alternativ förblir enkelheten och integrationen av Google Apps Scripts inbyggda datum- och tidsfunktioner ett förstahandsval för de flesta uppgifter inom Google-ekosystemet.
