---
title:    "Bash: Stor Bokstavering av en sträng"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför

Att kunna manipulera textsträngar är en grundläggande färdighet inom programmering, och att kunna konvertera en sträng till att börja med en stor bokstav kan vara mycket användbart vid formatering av data eller utskrifter. I denna bloggpost kommer vi att gå igenom hur du kan göra detta i Bash-programmering.

## Så här gör du

För att kapitalisera en sträng i Bash, kan du använda kommandot `tr` tillsammans med `[:lower:]` och `[:upper:]` för att ändra tecken från små till stora bokstäver. Här är ett exempel på hur du kan implementera detta:

```Bash
strang="hej, jag heter Swedish reader."
kapitaliserad_strang=$(echo $strang | tr '[:lower:]' '[:upper:]')
echo $kapitaliserad_strang
```

Detta kommer att producera följande utgång:

```
HEJ, JAG HETER SWEDISH READER.
```

Som du kan se har alla små bokstäver i den ursprungliga strängen nu konverterats till stora bokstäver. Detta är en enkel och effektiv metod för att kapitalisera en sträng i Bash.

## Djupdykning

Förutom `tr`-kommandot finns det också andra sätt att kapitalisera en sträng i Bash. Du kan till exempel använda inbyggda funktioner som `tr a-z A-Z` för att uppnå samma resultat. Dessutom finns det möjlighet att använda reguljära uttryck för att manipulera och hämta specifika tecken i en sträng.

Det är också viktigt att notera att detta bara kommer att påverka det första tecknet i strängen. Om du vill kapitalisera varje ord i en sträng, kan du använda loopen `for` tillsammans med `cut`, `tr` och `sed` för att manipulera varje ord individuellt.

## Se även

Här är några användbara länkar för vidare studier av Bash-programmering och manipulering av strängar:

- [Bash scripting tutorial](https://ryanstutorials.net/bash-scripting-tutorial/bash-if-statements.php)
- [Manipulating strings in Bash](https://www.linuxjournal.com/article/8919)
- [Regular expressions in Bash](https://regex101.com/r/z9ruDY/1)