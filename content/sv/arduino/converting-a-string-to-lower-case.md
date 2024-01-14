---
title:                "Arduino: Omvandla en sträng till gemener"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Det finns många situationer där det kan vara användbart att konvertera en sträng till gemener (lower case) i Arduino-programmering. Det kan till exempel behövas för att jämföra strängar eller formatera utdata på ett enhetligt sätt.

## Hur man gör det

För att konvertera en sträng till gemener i Arduino finns det flera olika metoder att använda sig av, beroende på behov och preferenser. Ett enkelt sätt är att använda sig av den inbyggda funktionen toLowerCase(), som omvandlar alla bokstäver i en sträng till gemener. Nedan finns ett exempel på hur detta skulle kunna se ut i kod:

```Arduino
String original = "HEJ";

String converted = original.toLowerCase();

Serial.println(converted);   // Utdata: hej 
```

Man kan också använda sig av en for-loop för att gå igenom varje tecken i strängen och omvandla det individuellt. Det ser ut så här:

```Arduino
String original = "PROGRAMMERING";

String converted = "";

for (int i = 0; i < original.length(); i++) {
    char letter = original.charAt(i);
    converted += toLowerCase(letter);
}

Serial.println(converted);    // Utdata: programmering 
```

## Djupdykning

När du konverterar en sträng till gemener är det viktigt att vara medveten om att detta enbart fungerar för tecken i ASCII-tabellen. Om du behöver konvertera andra tecken kan du använda dig av en array av bokstäver och motsvarande gemener för att jämföra och byta ut dem. Detta kan också behövas om du använder dig av specialtecken i ditt projekt. Det finns också andra funktioner, som toLowerCase(char) och toLowerCase(int), som gör att du kan konvertera enstaka tecken istället för hela strängar.

## Se även

- Utforska den inbyggda funktionen toLowerCase() för andra användningsområden: https://www.arduino.cc/reference/en/language/functions/string/functions/tolowercase/
- Lär dig mer om hur man använder for-loopar i Arduino: https://www.arduino.cc/reference/en/language/structure/control-structure/for/