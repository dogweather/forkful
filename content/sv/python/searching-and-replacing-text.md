---
title:    "Python: Sökning och utbyte av text"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
Att söka och ersätta text är en vanlig uppgift inom programmering, särskilt när man arbetar med bearbetning av stora mängder data eller skapar användarvänliga gränssnitt. Genom att förstå grunderna i sök- och ersättningsfunktionerna i Python kan du effektivt manipulera text efter dina behov.

## Hur man gör
För att söka och ersätta text i Python kan du använda dig av strängmetoder som `.replace()` eller reguljära uttryck med modulen `re`. Här är några exempel på hur du kan använda dessa funktioner för att söka och ersätta text:

```Python
# Använda .replace()
text = "Hej, jag heter Anna och jag gillar katter"
ny_text = text.replace("Anna", "Emma") # "Hej, jag heter Emma och jag gillar katter"

# Använda reguljära uttryck
import re

# Ersätta siffror med X
text = "123-456-789"
ny_text = re.sub(r"[0-9]+", "X", text) # "X-X-X"

# Ta bort alla skiljetecken och mellanslag
text = "Det här är en text med många skiljetecken, som punkter, kommatecken och frågetecken."
ny_text = re.sub(r"[^\w\s]","", text) # "Dethärentextmedmångaskiljeteckensompunkterkommateckenochfrågetecken"
```

Som du kan se i kodexemplen ovan krävs det första att du definierar en variabel för den ursprungliga texten och sedan tilldelar en ny variabel värdet av `.replace()`- eller `re.sub()`-funktionen beroende på vilken metod du väljer att använda. I de reguljära uttrycken måste du också inkludera modulen `re` och använda `r` före ditt uttryck för att markera det som en "rå" sträng.

## Djupdykning
För att förstå de olika möjligheterna när det gäller att söka och ersätta text är det bra att veta mer om reguljära uttryck och deras syntax. Reguljära uttryck används för att matcha mönster i text, vilket gör det möjligt att hitta och ersätta vissa delar av en sträng. De använder speciella metatecken för att uttrycka mönster, till exempel `.` för att matcha alla tecken, `+` för att matcha en eller flera gånger och `[...]` för att matcha en specifik uppsättning tecken. Genom att lära dig mer om reguljära uttryck kan du anpassa dina sök- och ersättningsfunktioner efter dina specifika behov.

## Se även
Här är några resurser där du kan lära dig mer om sök- och ersättningsfunktioner i Python:
- [Python String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Regular Expression HOWTO](https://docs.python.org/3/howto/regex.html)
- [Regular Expressions Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/python)