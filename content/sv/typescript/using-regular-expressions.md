---
title:                "TypeScript: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
Regex, eller reguljära uttryck, är ett kraftfullt verktyg inom programmering som hjälper till att söka och manipulera textsträngar. Detta kan vara särskilt användbart vid hantering av användarinput eller analys av stora datamängder.

## Hur man använder det 
```TypeScript
// Skapa ett reguljärt uttryck för att matcha en e-postadress
let emailRegex = /^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}$/i;

// Sök efter en matchning i en textsträng
let result = emailRegex.exec("exempel@email.com");

// Om det finns en matchning, skriv ut den
if (result) {
    console.log("Matchande e-postadress hittad: " + result[0]);
} else {
    console.log("Ingen matchande e-postadress hittades.");
}
```

Output:
```
Matchande e-postadress hittad: exempel@email.com
```

## Djupdykning
Reguljära uttryck kan tyckas komplicerade till en början, men en gång förstådda kan de vara mycket användbara. Här är några viktiga saker att tänka på när du använder regex i TypeScript:

- ```/``` används för att starta och avsluta ett reguljärt uttryck
- ```^``` matchar början av en sträng
- ```$``` matchar slutet av en sträng
- ```[...]``` matchar vilket tecken som helst inom klammerparenteserna
- ```+``` matchar ett eller flera förekomster av föregående uttryck
- ```*``` matchar noll eller flera förekomster av föregående uttryck
- ```?``` matchar zero eller one förekomster av föregående uttryck
- ```\``` används för att escape:a specialtecken som skulle tolkas som en del av ett reguljärt uttryck
- ```i``` används för att ignorera skillnaden mellan versaler och gemener

För att lära dig mer, kolla in Microsofts dokumentation om reguljära uttryck i TypeScript eller öva på regex101.com.

## Se även
- [Microsofts dokumentation om reguljära uttryck i TypeScript](https://docs.microsoft.com/sv-se/scripting/javascript/regular-expressions-javascript)
- [Övning på regex101.com](https://regex101.com/)