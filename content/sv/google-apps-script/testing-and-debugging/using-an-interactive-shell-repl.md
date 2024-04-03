---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:19.922474-07:00
description: "Hur: Google Apps Script, ett molnbaserat skriptspr\xE5k f\xF6r automatisering\
  \ av uppgifter \xF6ver Googles produkter, har inte ett inbyggt REPL-verktyg liknande\
  \ de\u2026"
lastmod: '2024-03-13T22:44:37.440518-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, ett molnbaserat skriptspr\xE5k f\xF6r automatisering\
  \ av uppgifter \xF6ver Googles produkter, har inte ett inbyggt REPL-verktyg liknande\
  \ de i spr\xE5k som Python eller JavaScripts Node.js."
title: "Anv\xE4nda en interaktiv skal (REPL)"
weight: 34
---

## Hur:
Google Apps Script, ett molnbaserat skriptspråk för automatisering av uppgifter över Googles produkter, har inte ett inbyggt REPL-verktyg liknande de i språk som Python eller JavaScripts Node.js. Du kan dock simulera en liknande upplevelse genom att använda Apps Script-redigerarens logg- och felsökningsfunktioner eller genom att sätta upp en extern miljö. Här fokuserar vi på att skapa en provisorisk REPL inuti Apps Script-redigeraren.

1. **Skapa en provisorisk REPL-funktion**:

```javascript
function myREPL() {
  var input = Logger.log('Ange ditt uttryck: ');
  försök {
    var resultat = eval(input);
    Logger.log('Resultat: ' + resultat);
  } fånga(e) {
    Logger.log('Fel: ' + e.message);
  }
}
```

Eftersom direkt användarinmatning inte är genomförbar på samma sätt som en traditionell REPL i Apps Script-miljön, kan du manuellt modifiera `input`-variabeln och köra `myREPL()` för att testa uttryck.

2. **Exempel på kodexekvering**:

Låt säga att du vill utvärdera `2+2`. Du skulle modifiera `myREPL`-funktionen på följande sätt:

```javascript
function myREPL() {
  var input = '2+2'; // Ange ditt uttryck manuellt här
  // Resten förblir densamma...
}
```

Efter att ha kört `myREPL()`, kontrollera Loggarna (Visa > Loggar) för utmatningen, som bör läsa något sådant som:

```
[20-xx-xxxx xx:xx:xx:xxx] Ange ditt uttryck:
[20-xx-xxxx xx:xx:xx:xxx] Resultat: 4
```

3. **Felsökning med Logger**:

För mer komplex felsökning, strö `Logger.log(variabel);` inom din kod för att skriva ut variabel tillstånd, vilket hjälper dig att förstå flödet och mellanliggande tillstånden i dina script.

## Fördjupning
Konceptet med en REPL är djupt rotad i databehandlingens historia, härstammande från tidsdelningssystemen från 1960-talet som tillät interaktiva sessioner. Språk som Lisp blomstrade i denna miljö, eftersom REPL var avgörande för deras iterativa utvecklingsprocess. I kontrast, Google Apps Script, som dök upp mycket senare, är främst designat för webben och fokuserar på att automatisera uppgifter inom Googles svit snarare än iterativ, konsolbaserad programmering.

Google Apps Script stöder traditionellt sett inte realtids, interaktiva kodningssessioner direkt ur lådan på grund av dess molnbaserade natur och fokus på webbapplikationsdistribution. Dess exekveringsmodell kretsar kring funktioner utlösta av webbhändelser, tidsdrivna triggers, eller manuell anropning inom miljön, snarare än omedelbara återkopplingsloopar som tillhandahålls av en REPL.

Medan den provisoriska REPL och felsökaren inom Apps Script-redigeraren erbjuder någon nivå av interaktivitet, replikerar de inte helt den omedelbara återkopplingen och effektiviteten hos traditionella REPLs som finns i många programmeringsspråk. Utvecklare som söker en mer autentisk REPL-upplevelse med Googles teknologier kan utforska externa JavaScript-miljöer eller Node.js med Googles API:er. Dessa kan erbjuda en mer responsiv och interaktiv kodningssession, om än krävande mer uppsättning och potentiellt att stiga utanför den direkta Apps Script-miljön.
