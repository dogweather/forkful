---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:25.694988-07:00
description: "Fels\xF6kning i Google Apps Script (GAS) inneb\xE4r processen att identifiera\
  \ och avl\xE4gsna fel fr\xE5n skript avsedda att automatisera Google Appar eller\
  \ bygga\u2026"
lastmod: '2024-03-13T22:44:37.443732-06:00'
model: gpt-4-0125-preview
summary: "Fels\xF6kning i Google Apps Script (GAS) inneb\xE4r processen att identifiera\
  \ och avl\xE4gsna fel fr\xE5n skript avsedda att automatisera Google Appar eller\
  \ bygga\u2026"
title: "Att anv\xE4nda en debugger"
---

## Hur man gör:
Google Apps Script erbjuder en inbyggd felsökare inom Apps Script Editor för att hjälpa till att felsöka skript. Så här initierar och använder du felsökaren:

1. **Öppna ditt skript i Apps Script Editor.**
2. **Välj en funktion att felsöka.** Från rullgardinsmenyn längst upp, välj den funktion du vill felsöka.
3. **Ställ in brytpunkter.** Klicka på marginalen (det grå området till vänster om radnumren) där du vill pausa exekveringen; en röd prick dyker upp, vilket indikerar en brytpunkt.
4. **Starta felsökningen.** Klicka på buggikonen eller välj `Debug` > `Starta felsökning`. Utförandet kommer att starta och pausa vid den första brytpunkten.

Betrakta detta enkla skript:

```javascript
function calculateSum() {
  var a = 5;
  var b = 10;
  var sum = a + b;
  Logger.log(sum); // Avsett att logga 15
}
```

Om du är osäker på varför `Logger.log(sum)` inte visar det förväntade resultatet, kunde du sätta en brytpunkt vid raden `var sum = a + b;` och stega igenom skriptet rad för rad för att inspektera variabelvärden.

**Exempelutdata i Logger:**

```plain
15
```

Under felsökningen tillåter Apps Script Editor dig att:

- **Stega genom koden** med hjälp av stega över, stega in i, och stega ut ur knapparna.
- **Övervaka uttryck och variabler** för att se deras värden förändras i realtid.
- **Inspektera anropsstacken** för att spåra funktionsanrop.

## Fördjupning
Felsökning i Google Apps Script, liksom i vilken annan programmeringsmiljö som helst, är avgörande för att skapa felfria applikationer. Introducerad tidigt i utvecklingen av GAS erbjuder den inbyggda felsökaren grundläggande funktioner för att inspektera och åtgärda kod inkrementellt. Medan den erbjuder grundläggande felsökningsfunktioner liknande de som finns i mer mogna miljöer som Visual Studio Code eller IntelliJ, kan den vara begränsad för komplexa felsökningsscenarier. Till exempel kan dess förmåga att inspektera asynkrona återanrop eller hantera tunga skriptexekveringar vara begränsande.

För komplexa felsökningsbehov kan utvecklare behöva ta till alternativa metoder såsom omfattande loggning (med `Logger.log()`) eller till och med distribuera som en webbapp för att inspektera beteende i ett verkligt scenario. Dock är enkelheten och integrationen av GAS:s felsökare inom Apps Script Editor ett ovärderligt första steg för felsökning och förståelse av skriptbeteende. Märkbart är att med Googles kontinuerliga uppdateringar och förbättringar till Apps Script förbättras felsökningsupplevelsen stadigt och erbjuder mer sofistikerade verktyg och alternativ över tiden. Denna utveckling återspeglar Googles åtagande att göra Apps Script till en kraftfullare och mer tillgänglig plattform för utvecklare från olika bakgrunder.
