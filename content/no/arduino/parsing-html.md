---
title:                "Analyse av HTML"
date:                  2024-01-20T15:29:55.427269-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av HTML betyr å tolke og bearbeide koden som nettlesere bruker for å vise websider. Programmere gjør dette for å trekke ut data, interagere med nettsider eller integrere webinnhold i egne applikasjoner.

## Slik gjør du:
Arduino har ikke innebygd støtte for HTML-parsing, men du kan bruke tekstbehandlingsteknikker for å hente ut informasjon. Her er et enkelt eksempel:

```Arduino
String html = "<h1>Title</h1><p>This is a paragraph.</p>";
String title = html.substring(html.indexOf("<h1>") + 4, html.indexOf("</h1>"));
String paragraph = html.substring(html.indexOf("<p>") + 3, html.indexOf("</p>"));

void setup() {
  Serial.begin(9600);
  while (!Serial) continue; // vent på seriell port
  Serial.println("Tittel: " + title);
  Serial.println("Avsnitt: " + paragraph);
}

void loop() {
  // Ikke nødvendig for dette eksemplet.
}
```

## Dypdykk
HTML-parsing på enheter som Arduino har historisk vært begrenset på grunn av begrenset minne og prosesseringskraft. Programmerere har tradisjonelt brukt server-side språk som Python eller PHP for slike oppgaver. Biblioteket `String` på Arduino kan brukes for enkel tekstmanipulasjon, men for kompleks HTML kan det være bedre å bruke en dedikert mikrokontroller med en nettverksstack som ESP8266 eller ESP32 som kan kjøre kraftigere parsingbiblioteker. 

Det finnes grunnleggende alternativer som regex (regulære uttrykk), selvom det ikke anbefales for kompleks HTML-parsing på grunn av HTMLs ofte uregelmessige natur. En mer robust tilnærming involverer bruk av en ekstern tjeneste eller API for å sende HTML og motta strukturert data tilbake, ta av belastningen fra Arduino.

## Se Også:
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [ESP8266 NodeMCU HTTP Client](https://randomnerdtutorials.com/esp8266-nodemcu-http-get-post-arduino/)
