---
title:                "Omvandling av en sträng till gemener"
html_title:           "Arduino: Omvandling av en sträng till gemener"
simple_title:         "Omvandling av en sträng till gemener"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När man arbetar med programmering så är det ibland nödvändigt att omvandla en textsträng till små bokstäver. Detta kallas för att konvertera en sträng till lower case. Det är viktigt att hålla strängar i samma format för att undvika problem och för att förenkla jämförelser.

## Hur man gör:
För att konvertera en sträng till lower case i Arduino, använd funktionen ```toLowerCase()```. Nedan är ett exempel på hur man använder denna funktion i en for-loop som går igenom alla bokstäver i en sträng och konverterar dem.

```Arduino
String str = "HELLO WORLD";

for (int i = 0; i < str.length(); i++) {
  str[i] = toLowerCase(str[i]);
}

Serial.println(str); // Output: hello world
```

## Djupdykning:
Historiskt sett har konvertering av strängar till lower case varit en utmaning när det kommer till teckenkodningar. I vissa teckenkodningar kunde en enkel byte inte representera alla bokstäver i alfabetet, vilket ledde till komplexa algoritmer för att omvandla bokstäver till lower case. Idag är detta mindre av en utmaning på grund av vanliga teckenkodningar som Unicode.

Det finns också andra sätt att konvertera strängar till lower case, såsom att använda en inbyggd funktion i ditt operativsystem eller använda ett tredjepartsbibliotek. Men i Arduino är ```toLowerCase()``` den enklaste och mest effektiva lösningen.

När det kommer till implementationen av ```toLowerCase()```, så använder Arduino en förändringsfunktion som byter ut varje bokstavs bytevärde till dess motsvarande lower case-värde.

## Se även:
Här är en länk till Arduino's dokumentation om ```toLowerCase()``` funktionen: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/