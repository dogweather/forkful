---
title:                "Söka och ersätta text"
html_title:           "Javascript: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Sökning och ersättning av text är en vanlig uppgift för programmerare. Det innebär helt enkelt att söka efter en specifik sträng av text inom en större text, och ersätta den med en annan sträng. Detta kan vara användbart för att göra ändringar i stora mängder av kod eller text.

Programmerare utför detta för att effektivisera sitt arbete och spara tid. Istället för att manuellt ändra text i flera filer, kan man använda sök- och ersättningsfunktioner för att göra ändringarna på en gång.

## Så här:

Enklaste sättet att utföra en sökning och ersättning i Javascript är genom att använda den inbyggda ```replace``` funktionen. Till exempel:

```
var originalText = "Hej alla!";
var nyText = originalText.replace("alla", "vänner");
console.log(nyText);
```

Output: "Hej vänner!"

Man kan också använda reguljära uttryck för att söka efter specifika mönster av text och ersätta dem. Till exempel:

```
var originalText = "Jag älskar att programmera i Javascript";
var nyText = originalText.replace(/javascript/i, "Python");
console.log(nyText);
```

Output: "Jag älskar att programmera i Python"

## Djupdykning:

Sökning och ersättning av text har funnits sedan de tidiga dagarna av datorprogrammering. Tidigare användes ofta komplicerade sökalgoritmer för att hitta specifika mönster av text. Idag är det enkelt att utföra genom inbyggda funktioner i olika programmeringsspråk.

En alternativ metod för att utföra sökning och ersättning i Javascript är att använda reguljära uttryck. Detta ger en mer flexibel och kraftfull metod för att hitta och ersätta text, men kan också vara mer komplicerat för nybörjare.

## Se även:

- [W3Schools: JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)