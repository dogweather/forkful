---
title:                "Använda reguljära uttryck"
date:                  2024-02-01T22:05:09.638947-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda reguljära uttryck"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/google-apps-script/using-regular-expressions.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Reguljära uttryck (regex) är mönster som används för att matcha teckenkombinationer i strängar. Programmerare använder dem för att söka, redigera eller manipulera text och data, vilket gör dem ovärderliga för mönstermatchning och tolkning av data.

## Hur man gör:

Att använda reguljära uttryck i Google Apps Script är enkelt tack vare syntaxen baserad på JavaScript. Här är hur du kan införliva regex i dina skript för vanliga uppgifter som sökning och datavalidering.

### Söka i Strängar

Låt oss säga att du vill hitta om en sträng innehåller ett specifikt mönster, som en e-postadress. Här är ett enkelt exempel:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("Hittad: " + found[0]);
  } else {
    Logger.log("Ingen e-post hittad.");
  }
}

// Exempelanvändning
findEmailInText("Kontakta oss på info@example.com.");
```

### Datavalidering

Reguljära uttryck utmärker sig inom datavalidering. Nedan är en funktion som validerar en inmatningssträng för att kontrollera om den följer en enkel lösenordspolicy (minst en stor bokstav, en liten bokstav och minst 8 tecken).

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// Exempel på utskrift
Logger.log(validatePassword("Str0ngPass")); // Ger: true
Logger.log(validatePassword("weak"));       // Ger: false
```

## Fördjupning

Reguljära uttryck i Google Apps Script är ärvt från JavaScript, först standardiserat i ECMAScript-språkspecifikationen i juni 1997. Även om de är kraftfulla kan de ibland leda till förvirrande och svårhanterlig kod, särskilt när de används för mycket eller för komplexa mönstermatchningsuppgifter som kanske kan lösas mer effektivt genom andra tolkningsmetoder.

Till exempel, även om du kan använda regex för att tolka HTML eller XML i nödfall, avråds det vanligtvis på grund av de nästlade och intrikata strukturerna hos dessa dokument. Istället är verktyg som specifikt är utformade för att tolka sådana strukturer, som DOM-parsare för HTML, mer tillförlitliga och läsbara.

Dessutom bör Google Apps Script-utvecklare vara medvetna om potentiella prestandaproblem när de använder komplexa regex-mönster i uppgifter för textmanipulering i stor skala, eftersom bearbetning av regex kan vara CPU-intensivt. I sådana fall kan det vara bättre att bryta ner uppgiften i enklare deluppgifter eller använda inbyggda strängmanipuleringsfunktioner för en bättre balans mellan prestanda och underhållbarhet.
