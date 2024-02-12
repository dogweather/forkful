---
title:                "Att starta ett nytt projekt"
aliases: - /sv/google-apps-script/starting-a-new-project.md
date:                  2024-02-01T22:02:46.215722-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att starta ett nytt projekt"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/google-apps-script/starting-a-new-project.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att starta ett nytt projekt i Google Apps Script (GAS) innebär att man initialiserar en skriptfil inom Googles ekosystem (Google Drive, Dokument, Kalkylark osv.) för att automatisera uppgifter eller utöka funktionaliteten i Google Apps. Programmerare ger sig ofta in på denna resa för att effektivisera arbetsflöden, manipulera Googles tjänster programmatiskt, eller skapa anpassade tillägg, vilket sparar tid och utnyttjar kraften i Googles infrastruktur.

## Hur man gör:

För att starta ett nytt projekt i Google Apps Script har du ett par ingångspunkter, men låt oss fokusera på den mest direkta metoden: att skapa ett skript från Google Drive.

1. **Skapa ett projekt i Google Drive**
   - Navigera till Google Drive (drive.google.com).
   - Klicka på "+ Nytt" > "Mer" > "Google Apps Script".
   - Ett nytt skriptprojekt öppnas i redigeraren. Som standard innehåller det en `Code.gs`-fil med ett exempel `myFunction`.

2. **Ställa in ditt projekt**
   - Byt namn på ditt projekt för tydlighetens skull. Klicka på "Ej namngivet projekt" längst upp till vänster och ge det ett betydelsefullt namn.
   - Skriv en enkel funktion i `Code.gs`-filen för att få en känsla av det:

```javascript
function helloWorld() {
  Logger.log('Hej, världen!');
}
```

   - Kör `helloWorld` genom att välja funktionen i rullgardinsmenyn bredvid spelknappen (▶) och klicka på den. Detta kommer att exekvera funktionen.

3. **Visa loggar**
   - För att se utdata från `Logger.log`, gå till "Visa" > "Loggar", eller tryck `Ctrl + Enter`. Du bör se "Hej, världen!" i loggarna.

Grattis, du har just framgångsrikt startat ett nytt projekt i Google Apps Script och kört en enkel funktion!

## Fördjupning

Lanseringen av Google Apps Script runt 2009 erbjöd en kraftfull, men lättillgänglig plattform för både utvecklare och icke-utvecklare att automatisera, utöka och bygga vidare på den stora mängden av Googles tjänster. Till skillnad från traditionella programmeringsmiljöer, erbjuder GAS en unik blandning av enkelhet och integration, direkt inom Googles ekosystem, utan behov av externa servrar eller installation. Den serverlösa exekveringsmodellen förenklar avsevärt projektimplementering och hantering.

Historiskt sett var GAS något begränsat av sin exekveringsmiljö och versionsversion av språket, ofta efterblivet jämfört med de nuvarande JavaScript-standarderna. Dock har nyligen uppdateringar medfört modern JavaScript-syntax (ECMAScript 2015+) till GAS, vilket gör det mer tilltalande för utvecklare vana vid nutida utvecklingspraxis.

Även om GAS är unikt positionerat för att interagera med Google-tjänster, finns det alternativa metoder för mer intensiva eller specifika behov. Till exempel erbjuder Google Cloud Functions och Google Cloud Platform (GCP) mer robusta och skalbara lösningar för hantering av komplexa arbetsflöden, bearbetning av stora datamängder och integration med externa API:er. Dessa plattformar tillåter programmering i olika språk (t.ex. Python, Go, Node.js) och erbjuder större beräkningsresurser.

Dock, för uppgifter intimt kopplade till Google Apps, automatisering och snabb utveckling inom detta ekosystem, förblir Google Apps Script ett oöverträffat verktyg när det gäller enkel användning och integrationsdjup. Dess tillgänglighet direkt från Google Drive och sömlösa anslutning till Googles tjänster gör det till ett praktiskt val för en mängd projekt, särskilt för de som vill utöka funktionaliteten i Kalkylark, Dokument, Formulär och andra Google-applikationer.
