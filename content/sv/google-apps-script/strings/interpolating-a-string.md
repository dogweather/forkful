---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:22.951935-07:00
description: "Str\xE4nginterpolering i Google Apps Script m\xF6jligg\xF6r dynamisk\
  \ inb\xE4ddning av uttryck inom str\xE4ngar, vilket underl\xE4ttar skapandet av\
  \ mer l\xE4sbar och\u2026"
lastmod: '2024-03-13T22:44:37.422727-06:00'
model: gpt-4-0125-preview
summary: "Str\xE4nginterpolering i Google Apps Script m\xF6jligg\xF6r dynamisk inb\xE4\
  ddning av uttryck inom str\xE4ngar, vilket underl\xE4ttar skapandet av mer l\xE4\
  sbar och underh\xE5llbar kod."
title: "Interpolering av en str\xE4ng"
weight: 8
---

## Vad & Varför?

Stränginterpolering i Google Apps Script möjliggör dynamisk inbäddning av uttryck inom strängar, vilket underlättar skapandet av mer läsbar och underhållbar kod. Programmerare använder denna teknik för att sömlöst infoga variabler och uttryck i strängar utan den omständliga konkateneringssyntaxen.

## Hur man gör:

I Google Apps Script uppnås stränginterpolering genom malliteraler. Dessa är strängliteraler som tillåter inbäddade uttryck, betecknade av backticks (\`) istället för de vanliga citattecknen. Så här kan du använda dem:

```javascript
// Ett grundläggande exempel
function basicInterpolationExample() {
  const user = 'Alice';
  console.log(`Hej, ${user}!`); // Utmatning: Hej, Alice!
}

// Att använda uttryck
function expressionInterpolationExample() {
  const a = 5;
  const b = 10;
  console.log(`Fem plus tio är ${a + b}.`); // Utmatning: Fem plus tio är 15.
}

// Flera rader strängar
function multiLineStringExample() {
  const item = 'Google Apps Script';
  console.log(`Detta är en flerlinjesträng:
Hej alla,
Vi diskuterar ${item} idag.`);
  // Utmatning:
  // Detta är en flerlinjesträng:
  // Hej alla,
  // Vi diskuterar Google Apps Script idag.
}

basicInterpolationExample();
expressionInterpolationExample();
multiLineStringExample();
```

Dessa exempel illustrerar grundläggande användning, inbäddning av uttryck och skapande av flerlinjesträngar med interpolerade värden.

## Djupdykning

Malliteraler, inklusive stränginterpolering, introducerades i ECMAScript 2015 (ES6) och antogs därefter i Google Apps Script. Innan detta var programmerare tvungna att förlita sig rent på strängkonkatenering, vilket kunde bli otympligt för komplexa strängar eller när man integrerade många variabelvärden.

```javascript
// Gammalt sätt (före ES6)
var user = 'Bob';
console.log('Hej, ' + user + '!');
```

Medan stränginterpolering är en kraftfull funktion, är det viktigt att vara medveten om de sammanhang där den används. Exempelvis kan direkt inbäddning av användarinput utan korrekt sanering leda till säkerhetsproblem, såsom injektionsattacker. Utvecklare av Google Apps Script bör se till att allt dynamiskt innehåll som interpoleras i strängar är korrekt kontrollerat eller sanerat.

Jämfört med andra programmeringsspråk existerar konceptet med stränginterpolering brett, med varierande syntax. Python använder f-strängar eller `format`-metoden, Ruby använder `#{}` inom dubbelciterade strängar, och många moderna språk har antagit liknande funktioner på grund av den läsbarhet och bekvämlighet de erbjuder.

Även om Google Apps Script inte erbjuder ytterligare interpoleringsfunktioner utöver de som tillhandahålls av ECMAScript-standarder, är funktionaliteten som finns kraftfull och tillräcklig för de flesta användningsfall. Utvecklare som kommer från språk med mer utarbetade interpoleringsmekanismer kanske behöver justera sina förväntningar men kommer sannolikt att uppskatta enkelheten och effektiviteten av malliteraler i Google Apps Script.
