---
aliases:
- /sv/google-apps-script/writing-tests/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:31.392926-07:00
description: "Att skriva tester i Google Apps Script (GAS) handlar om att skapa automatiserade\
  \ skript f\xF6r att verifiera beteendet hos din kod, och s\xE4kerst\xE4lla att den\u2026"
lastmod: 2024-02-18 23:08:51.382501
model: gpt-4-0125-preview
summary: "Att skriva tester i Google Apps Script (GAS) handlar om att skapa automatiserade\
  \ skript f\xF6r att verifiera beteendet hos din kod, och s\xE4kerst\xE4lla att den\u2026"
title: Skriva tester
---

{{< edit_this_page >}}

## Vad och varför?

Att skriva tester i Google Apps Script (GAS) handlar om att skapa automatiserade skript för att verifiera beteendet hos din kod, och säkerställa att den fungerar som förväntat under olika förhållanden. Programmerare gör det för att upptäcka buggar tidigt, förbättra kodkvaliteten och underlätta uppdateringar och underhåll.

## Hur man gör:

Även om Google Apps Script inte har ett inbyggt testramverk som vissa andra programmeringsmiljöer, kan du fortfarande skriva och köra tester genom att använda enkla GAS-funktioner eller integrera externa testbibliotek såsom `QUnit`. Här är ett grundläggande exempel där en enkel GAS-funktion används för att testa en annan funktion i ditt skript:

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var resultat = add(2, 3);
  if (resultat !== 5) {
    throw new Error("Test misslyckades: add(2, 3) borde vara 5, men var " + resultat);
  } else {
    Logger.log("Test godkänt!");
  }
}
```

Att köra `testAdd()` kommer att logga "Test godkänt!" om `add`-funktionen fungerar korrekt, eller kasta ett fel om den inte gör det. För en mer sofistikerad ansats involverar att integrera QUnit med Google Apps Script några fler steg men erbjuder en kraftfull testmiljö. En exempeluppsättning för QUnit-test ser ut så här:

1. Inkludera QUnit-biblioteket i ditt projekt.
2. Skapa en test HTML-fil för att köra QUnit-testerna.
3. Skriv testfall med QUnits syntax.

Här är ett exempel som använder QUnit:

```javascript
// Inkludera QUnit genom att länka till det i en HTML-fil som används för att köra dina tester

QUnit.test("Testar add-funktionen", function (assert) {
  var resultat = add(2, 3);
  assert.equal(resultat, 5, "add(2, 3) borde returnera 5");
});
```

För att se resultaten, öppna HTML-filen inuti GAS Script Editor eller distribuera den som en webbapp.

## Fördjupning

Historiskt sett har testning i Google Apps Script till viss del förbisetts, troligtvis på grund av plattformens ursprung och huvudsakliga användningsområden som fokuserar på snabba, småskaliga automationsuppgifter snarare än stora applikationer. Som sådant erbjuder GAS inte samma robusta testramverk och verktyg som finns i mer traditionella programmeringsmiljöer. Dock har gemenskapen anpassat sig genom att inkludera open-source bibliotek och kreativt använda Googles befintliga verktyg.

Att använda bibliotek som QUnit representerar ett betydande steg framåt men kommer med sin egen uppsättning utmaningar, såsom att sätta upp en lämplig testmiljö och lära sig en ytterligare syntax. Dock, för de som är investerade i att bygga mer komplexa och tillförlitliga applikationer med GAS, är ansträngningen värt det.

Alternativ som att använda enkla GAS-funktioner för testning erbjuder enkel användning och integration med GAS-miljön utan ytterligare beroenden men saknar omfattande testfunktioner och möjligheten att enkelt skala upp allteftersom ditt projekt växer. Verktyg såsom clasp (Google Apps Script Command Line Interface) kan underlätta mer avancerade arbetsflöden, inklusive testning, genom att låta utvecklare koda i deras föredragna IDE, vilket introducerar möjlighet för integration med externa testramverk mer sömlöst.

Slutsatsen, medan GAS kanske inte har stöd för sofistikerad testning direkt ur lådan, ger dess flexibilitet och gemenskapens innovativa tillvägagångssätt genomförbara vägar för att säkerställa att dina skript är robusta, tillförlitliga och redo för vilken uppgift som helst.
