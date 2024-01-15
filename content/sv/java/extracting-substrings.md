---
title:                "Extrahering av delsträngar"
html_title:           "Java: Extrahering av delsträngar"
simple_title:         "Extrahering av delsträngar"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/extracting-substrings.md"
---

{{< edit_this_page >}}

##
Varför: Varför ska man engagera sig i att extrahera delsträngar? Några användbara exempel kan vara att manipulera och bearbeta data, hitta och ersätta specifika delar av en text eller för att utföra textanalys.

## Hur man gör

Det finns många tillvägagångssätt för att extrahera delsträngar i Java, men ett vanligt sätt är att använda metoden "substring ()", som finns tillgänglig för alla strängobjekt i Java. 

```Java
// Skapa en sträng som vi vill extrahera en del av
String originalStrang = "Välkommen till Java-världen!";

// Använd substring-metoden för att extrahera text från en viss position till en annan
String extraheradStrang = originalStrang.substring(12, 17);

// Skriv ut den extraherade delen av strängen
System.out.println(extraheradStrang);

```

Output: "Java"

Som du kan se ovan, har vi använt "substring ()" -metoden för att extrahera delsträngen "Java" från vår ursprungliga sträng. Metoden tar två parametrar, startindex och slutindex för den del av strängen som ska extraheras. Det är viktigt att notera att slutindex inte inkluderas i den extraherade delen. 

```Java
// Skapa en sträng med flera ord
String originalStrang = "Detta är en lång mening";

// Splittra strängen vid mellanslag och lagra den i en array
String[] ord = originalStrang.split(" ");

// Använd substring-metoden för att extrahera ett specifikt ord
String extraheratOrd = ord[3].substring(0, 3);

// Skriv ut det extraherade ordet
System.out.println(extraheratOrd);

```

Output: "lån"

I det här exemplet har vi först använt "split ()" -metoden för att dela upp den ursprungliga strängen vid mellanslag och sedan använt "substring ()" för att extrahera det fjärde ordet ("lång") och bara behålla de första tre bokstäverna.

## Djupdykning

Förutom att använda substring-metoden finns det andra sätt att extrahera delsträngar beroende på vilken typ av manipulation du vill göra. Till exempel kan du använda "replace ()" -metoden för att byta ut en delsträng med en annan, eller "indexOf ()" -metoden för att hitta den första positionen för en delsträng i en större sträng. 

Det är också möjligt att kombinera flera strängmetoder för att uppnå mer komplexa extraktioner. Det viktigaste att komma ihåg är att vara försiktig med vilka index och gränser du använder för att extrahera delsträngar, eftersom det kan leda till felaktiga resultat eller fel i din kod.

## Se också

- [Java String Class Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [W3Schools Java String Methods](https://www.w3schools.com/java/java_ref_string.asp)
- [GeeksforGeeks Java String Methods](https://www.geeksforgeeks.org/string-class-in-java/)