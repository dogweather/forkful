---
title:    "TypeScript: Hitta längden på en sträng"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

### Varför

Att hitta längden på en sträng är en grundläggande uppgift inom programmering och är användbart för många olika applikationer. Det kan hjälpa till att förstå och manipulera textdata, vilket är en viktig del av många program och webbapplikationer.

### Så här gör du

För att hitta längden på en sträng i TypeScript, kan du använda den inbyggda metoden `length`. Detta gör det enkelt att få fram antalet tecken i en sträng.

```TypeScript
let sträng = "Hej, jag heter Anna!";
console.log(sträng.length);
```

Detta kommer att ge utskrift av `19` eftersom det är det totala antalet tecken i strängen, inklusive mellanslag. Om vi ​​vill exkludera mellanslag, kan vi använda `trim` metoden för att ta bort dem innan vi använder `length` metoden.

```TypeScript
let sträng = "   Hej, jag heter Anna!   ";
console.log(sträng.trim().length);
```

Detta kommer att ge utskrift av `19` eftersom mellanslag har tagits bort och endast tecken kvarstår. Det är viktigt att komma ihåg att indexeringen av strängar i TypeScript börjar från 0, så den faktiska längden på en sträng är alltid indexet på sista tecknet plus 1.

### Djupdykning

En viktig skillnad mellan TypeScript och andra programmeringsspråk är att strängar inte kan ändras en gång de har skapats. Det beror på att strängar i TypeScript är immutabla, vilket innebär att de inte kan ändras. Om vi ​​försöker ändra en sträng kommer vi att få en felmeddelande. Detta är ett viktigt koncept att förstå vid hantering av strängar i TypeScript.

Det finns också andra metoder som kan användas för att manipulera strängar, som `slice` för att ta ut delar av en sträng och `concat` för att slå ihop flera strängar.

### Se även

* [TypeScript dokumentation om strängar](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
* [W3Schools artikel om strängmetoder i TypeScript](https://www.w3schools.com/jsref/jsref_obj_string.asp)