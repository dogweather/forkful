---
title:    "Java: Omvandla en sträng till gemener"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener (lower case) är en vanlig uppgift i programmering, speciellt inom textbehandling. Det kan vara användbart när man vill jämföra strängar, sortera dem i alfabetisk ordning eller bara för att få enhetlig formatering.

## Så här gör du

För att konvertera en sträng till gemener i Java kan man använda metoden `toLowerCase()`. Här är ett enkelt exempel:

```Java
String text = "HEJ ALLA SWEDEN";
System.out.println(text.toLowerCase());
```

Output: `hej alla sweden`

Man kan också använda denna metod tillsammans med andra strängmanipuleringsmetoder. Här är ett exempel där man först tar bort mellanslag från en sträng och sedan konverterar den till gemener:

```Java
String text = " DETTA ÄR EN STRÄNG MED MELLANSLAG ";
System.out.println(text.trim().toLowerCase());
```

Output: `detta är en sträng med mellanslag`

## Djupdykning

När man konverterar en sträng till gemener i Java, så kommer alla bokstäver att bli gemener oavsett deras ursprungliga formatering. Det innebär att även bokstäverna Å, Ä och Ö kommer att bli små. Detta kan vara viktigt att tänka på när man arbetar med speciella tecken i sin kod.

Det finns också andra metoder man kan använda för att konvertera mellan olika bokstavskapitaliseringar, som t.ex. `toUpperCase()` för att göra en sträng med stora bokstäver och `toTitleCase()` för att göra första bokstaven i varje ord till stor bokstav.

## Se även

- Java String API: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
- Tutorialspoint: https://www.tutorialspoint.com/java/lang/string_tolowercase.htm
- Oracle Java tutorials: https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html