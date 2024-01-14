---
title:    "Arduino: Att göra en sträng med stor bokstav"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Varför
I denna bloggpost kommer vi att utforska ett enkelt men användbart programmeringskoncept: att göra en sträng stor bokstav. Att kunna ändra mellan stor och liten stil på en sträng kan vara användbart i många olika situationer, från att formatera text till att verifiera användardata. Låt oss ta en titt på hur man gör det på Arduino!

## Så här gör du
För att göra en sträng stor bokstav på Arduino, behöver vi använda funktionen `toUpperCase()`. Den här funktionen tar en sträng som argument och byter ut alla små bokstäver mot motsvarande stora bokstäver. Låt oss se ett exempel:

```Arduino
String minStrang = "hej alla";
minStrang.toUpperCase();
Serial.println(minStrang);
```

Output:
```
HEJ ALLA
```

Som du kan se har funktionen `toUpperCase()` omvandlat alla små bokstäver till stora i vår sträng "hej alla". Det är viktigt att notera att den ursprungliga strängen inte ändras, utan en ny sträng skapas med de ändrade bokstäverna.

Det finns också en motsvarande funktion `toLowerCase()` som gör tvärtom, den byter ut alla stora bokstäver mot små.

```
## Deep Dive
Nu när vi har en grundläggande förståelse för hur man gör en sträng stor bokstav, låt oss ta en titt på hur det fungerar under ytan.

När vi använder funktionen `toUpperCase()`, går Arduino igenom varje tecken i strängen och jämför det med ASCII-tabellen. ASCII-tabellen innehåller numeriska värden för varje tecken, både stora och små bokstäver. Eftersom stora bokstäver har lägre numeriska värden än små bokstäver, är det enkelt för Arduino att byta ut dem.

Något att tänka på är att funktionen `toUpperCase()` inte fungerar med diakritiska tecken eller specialtecken som inte finns i ASCII-tabellen. I sådana fall skulle man behöva använda en annan funktion eller skriva en egen kod för att omvandla dessa tecken.

## Se även
- [ASCII-tabellen](https://sv.wikipedia.org/wiki/ASCII)
- [toUpperCase() referens](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)
- [toLowerCase() referens](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)