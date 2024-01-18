---
title:                "Analys av ett datum från en sträng"
html_title:           "Arduino: Analys av ett datum från en sträng"
simple_title:         "Analys av ett datum från en sträng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Att "parsa" ett datum från en sträng innebär i princip att omvandla en textrepresentation av ett datum till ett datumobjekt som kan användas av en dator. Programmers behöver ofta parsadetum från strängar för att kunna behandla datum i sina program, till exempel för att räkna ut tidsintervall eller jämföra datum.

## Så här gör du:

```Arduino
// Exempelkod för att parsadetum från en sträng
String strDatum = "2021-01-01"; // Strängen som ska parsas, i formatet ÅÅÅÅ-MM-DD
int ar = strDatum.substring(0, 4).toInt(); // Plockar ut året från strängen och konverterar det till ett heltal
int manad = strDatum.substring(5, 7).toInt(); // Plockar ut månaden från strängen
int dag = strDatum.substring(8, 10).toInt(); // Plockar ut dagen från strängen 
// Nu kan vi skapa ett datumobjekt med hjälp av de tre variablerna
tmElements_t datum = {0, 0, 0, dag, manad, ar};

// Exempel på hur datumet kan utskrivas som text
Serial.println("Parsat datum: " + String(datum.Day) + "." + String(datum.Month) + "." + String(datum.Year));

```

Output: `Parsat datum: 1.1.2021`

## Djupdykning:

Parsning av datum från strängar är en vanlig uppgift inom programmering. Det är särskilt användbart när man behöver behandla datum i olika format eller när man samarbetar med system som använder olika datumformat. Det finns också olika sätt att parsadetum från strängar, till exempel genom att använda inbyggda funktioner eller tredjepartsbibliotek. Implementeringen kan också variera beroende på vilket programmeringsspråk eller plattform man använder.

## Se även:

Här är några länkar till andra källor som kan vara intressanta för dig som vill lära dig mer om parsning av datum från strängar:
- [Dokumentation för Arduino's `toInt()` funktion](https://www.arduino.cc/reference/en/language/functions/conversion/toint/)
- [En guide om olika sätt att arbeta med datum i Arduino](https://www.instructables.com/Arduino-Dates/)