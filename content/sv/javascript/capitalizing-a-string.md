---
title:                "Att göra en sträng versal"
date:                  2024-01-19
simple_title:         "Att göra en sträng versal"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en sträng innebär att omvandla första bokstaven i varje ord till stor bokstav, medan resten av bokstäverna förblir små. Programmerare använder detta för att formatera texter så att titlar och namn följer konventionella skrivregler.

## Så Gör Du:
```javascript
function capitalizeString(str) {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

console.log(capitalizeString('en enkel sträng med ord')); // En Enkel Sträng Med Ord
```

## Djupdykning
Under historiens gång har olika programmeringsspråk hanterat strängar på olika sätt. I JavaScript har det inte funnits något inbyggt stöd för att kapitalisera en hel sträng, vilket ledde till att programmerare behövde skriva egna funktioner för detta. Alternativ till `replace`-metoden ovan inkluderar att använda bibliotek som Lodash, som har en `capitalize`-funktion för att enkelt hantera strängkapitalisering. Detaljer i implementeringen, såsom att använda reguljära uttryck som i exemplet ovan, skiljer sig från andra metoder som kan iterera över varje ord och kapitalisera det för sig.

## Se Också
- MDN Web Docs om strängar i JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String
- Lodash-dokumentation för `capitalize`-funktionen: https://lodash.com/docs/#capitalize
- Stack Overflow-diskussioner kring olika sätt att kapitalisera strängar: https://stackoverflow.com/search?q=capitalize+string+javascript
