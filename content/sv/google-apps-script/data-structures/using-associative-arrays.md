---
title:                "Använda associativa arrayer"
aliases:
- sv/google-apps-script/using-associative-arrays.md
date:                  2024-02-01T22:04:24.173640-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda associativa arrayer"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/google-apps-script/using-associative-arrays.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Associativa arrayer, kända som objekt i Google Apps Script (en variant av JavaScript), låter programmerare skapa samlingar av nyckel-värde-par. Denna funktionalitet är avgörande för att lagra och manipulera data effektivt, särskilt när man arbetar med dynamiskt namngivna egenskaper eller när en traditionell arrays linjära lagrings- och åtkomstmodell är otillräcklig.

## Hur:

I Google Apps Script skapar och manipulerar du associativa arrayer (objekt) genom att använda klammer `{}`, där du definierar nyckel-värde-par inuti. Nycklar är unika identifierare, och värden kan vara allt från strängar och nummer till mer komplexa objekt eller funktioner. Här är ett grundläggande exempel:

```javascript
function createAssociativeArray() {
  var user = {
    name: "John Doe",
    age: 30,
    email: "johndoe@example.com"
  };

  // Åtkomst av värden
  Logger.log(user.name); // Ger: John Doe
  Logger.log(user["email"]); // Ger: johndoe@example.com

  // Lägga till nya nyckel-värde-par
  user.title = "Software Developer";
  user["country"] = "USA";

  Logger.log(user.title); // Ger: Software Developer

  // Iterera över nyckel-värde-par
  for (var key in user) {
    Logger.log(key + ': ' + user[key]);
  }
}
```

Exempelutskrift för iterationen kan se ut så här:
```
name: John Doe
age: 30
email: johndoe@example.com
title: Software Developer
country: USA
```

Notera hur du kan använda både punktnotation och hakparentesnotation för att åtkomma och sätta egenskaper. Hakparentesnotation är särskilt användbart när man arbetar med nycklar som är dynamiskt bestämda eller inkluderar tecken som inte är tillåtna i identifierare.

## Djupdyk

Associativa arrayer i form av objekt har varit en hörnsten i JavaScript, och genom utvidgning i Google Apps Script, vilket återspeglar dess prototypbaserade arvs mekanism. Till skillnad från språk med traditionella associativa arrayer eller ordböcker (t.ex. Pythons dict), erbjuder Google Apps Script objekt ett flexibelt och kraftfullt sätt att strukturera data, med fördelar från JavaScripts dynamiska natur.

Det är dock viktigt att notera att ECMAScript 2015-specifikationen introducerade `Map` och `Set` objekt, som erbjuder en mer rakt på sak hantering av associativa samlingar med vissa fördelar över objekt, såsom att bibehålla införingsordningen och bättre prestanda för stora dataset. Även om Google Apps Script också stöder dessa, beror valet mellan att använda objekt eller de nyare `Map`/`Set` strukturerna på specifika behov och prestandaöverväganden. För de flesta uppgifter med associativa arrayer, ger traditionella objektbaserade implementationer ett bekant och mångsidigt tillvägagångssätt, men det är rådligt att undersöka nyare alternativ när komplexiteten i ditt skript ökar.
