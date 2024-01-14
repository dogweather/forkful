---
title:    "Java: Stor Bokstavering av en Sträng"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Varför

Att göra en sträng stor är en användbar funktion i många situationer, till exempel att korrigera stavning eller formatera text för en rubrik.

# Hur man gör

För att göra en sträng stor i Java, använder vi inbyggda metoden `toUpperCase()` tillsammans med en strängvariabel. Här är ett exempel på kod som visar detta:

```Java
//Skapar en strängvariabel
String meddelande = "välkommen till min blogg";

//Använder toUpperCase() metoden för att göra strängen stor
String storSträng = meddelande.toUpperCase();

//Skriver ut den stora strängen
System.out.println(storSträng);
```

Output: VÄLKOMMEN TILL MIN BLOGG

# Djupdykning

När vi använder `toUpperCase()` metoden, omvandlar vi alla bokstäver i en sträng till stora bokstäver oavsett om de var små eller stora innan. Denna metod är också språkkänslig, vilket betyder att resultatet kommer att variera beroende på vilket språk som används.

Det finns också flera andra metoder för att manipulera strängar i Java, såsom `toLowerCase()` som gör strängen liten, eller `substring()` som skapar en ny sträng baserad på en del av den ursprungliga strängen.

# Se även

- Java String Class: https://www.w3schools.com/java/java_strings.asp
- String toUpperCase() method: https://www.w3schools.com/java/ref_string_touppercase.asp