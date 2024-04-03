---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:44.695732-07:00
description: "Att skriva till standardfel (stderr) i programmeringsspr\xE5k handlar\
  \ om att dirigera felmeddelanden och diagnostik till en separat str\xF6m, skild\
  \ fr\xE5n den\u2026"
lastmod: '2024-03-13T22:44:37.456661-06:00'
model: gpt-4-0125-preview
summary: "Att skriva till standardfel (stderr) i programmeringsspr\xE5k handlar om\
  \ att dirigera felmeddelanden och diagnostik till en separat str\xF6m, skild fr\xE5\
  n den standardm\xE4ssiga utmatningen (stdout)."
title: Skriva till standardfel
weight: 25
---

## Hur:
Google Apps Script, som är ett skriptspråk för lättviktsapplikationsutveckling på Google Apps-plattformen, erbjuder inte en direkt inbyggd funktion som `console.error()` för skrivning till stderr, som man kanske hittar i Node.js eller Python. Dock kan du simulera detta beteende genom att använda Google Apps Scripts loggtjänster eller anpassad felhantering för att hantera och separera felutmatningar.

### Exempel: Användning av `Logger` för felmeddelanden
```javascript
function logError() {
  try {
    // Simulera ett fel
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("Försök till division med noll");
  } catch (e) {
    // Skriv felmeddelande till Loggar
    Logger.log('Fel: ' + e.message);
  }
}
```

När du kör `logError()`, kommer detta att skriva felmeddelandet till Google Apps Scripts logg, vilken du kan se genom `Visa > Loggar`. Detta är inte exakt stderr, men det tjänar ett liknande syfte att separera felloggar från standardutmatningar.

### Avancerad diagnostisk loggning
För mer avancerad felsökning och felloggning kan du använda Stackdriver Logging, nu känd som Google Cloud's Operations Suite.

```javascript
function advancedErrorLogging() {
  try {
    // Orsaka ett fel medvetet
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('Fel upptäckt: ', e.toString());
  }
}
```

Detta kommer att rikta felmeddelandet till Stackdriver Logging, där det hanteras som en logg på felnivå. Notera att integrationen av Stackdriver/Google Cloud's Operations Suite erbjuder en mer detaljerad och sökbar logglösning jämfört med `Logger`.

## Fördjupning
Avsaknaden av en dedikerad `stderr`-ström i Google Apps Script speglar dess natur och ursprung som ett molnbaserat skriptspråk, där traditionella konsol- eller terminalbaserade utmatningar (som stdout och stderr) är mindre relevanta. Historiskt var Google Apps Script utformat för att förbättra funktionaliteten i Google Apps med enkla skript, med fokus på användarvänlighet snarare än omfattande funktioner tillgängliga i mer komplexa programmeringsmiljöer.

Detta sagt, evolutionen av Google Apps Script mot mer avancerad applikationsutveckling har föranlett utvecklare att anta kreativa tillvägagångssätt för felhantering och loggning, genom att utnyttja tillgängliga tjänster som Logger och integrera med Google Cloud’s Operations Suite. Dessa metoder, även om de inte är direkta stderr-implementeringar, erbjuder robusta alternativ för felhantering och diagnostisk loggning i en molncentrisk miljö.

Kritiskt är att även om dessa metoder tjänar sitt syfte inom Google Apps Scripts ekosystem, understryker de plattformens begränsningar jämfört med traditionella programmeringsmiljöer. För utvecklare som kräver detaljerade och hierarkiska felhanteringsstrategier, kan integration med externa loggtjänster eller antagande av Google Cloud Functions, som erbjuder en mer konventionell hantering av stderr och stdout, vara att föredra.
