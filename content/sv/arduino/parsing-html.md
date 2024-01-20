---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka (parse) HTML betyder att omvandla en block av HTML-kodningar till ett format som är enklare att arbeta med programmeringsmässigt. Programmerare gör detta för att extrahera data, förändra innehållet, eller bygga webbskrapor.

## Hur Gör Man:
Här är ett exempel på hur du kan tolka (parse) HTML med hjälp av Arduino och biblioteket "Arduino-Xml":
```Arduino
#include <Arduino-Xml.h>

void setup() {
  Serial.begin(115200);
}

void loop() {
  const char* html = "<div><p>Hej Arduino!</p></div>";
  XmlDocument doc(256);
  doc.from(html);
  
  XmlNode* textNode = doc.getRoot()->getFirstChild()->getFirstChild();
  Serial.println(textNode->getValue());
}
```
Vid körning av det här programmet kommer utskriften på seriemonitorn att vara: "Hej Arduino!".

## Djupdykning
Historiskt sett var HTML-tolkning ett komplext problem för programmerare, eftersom HTML strukturer kan vara väldigt komplexa och oförutsägbara. Med introduktionen av Arduino-XML kan HTML nu lätt omvandlas till färdiga XML-strukturer som är lättare att bredda med vanliga programmeringstekniker.

Det finns andra bibliotek för att parse HTML, som Html-Parser (för C++), men Arduino-Xml erbjuder fler funktioner och en mer optimerad tolkning av HTML.

När du tolkar HTML på Arduino är det viktigt att övervaka din minnesanvändning. HTML-dokument kan snabbt äta upp resurser om de inte hanteras korrekt i stora applikationer eller spädbarn.

## Se Även
- [Arduino-Xml Biblioteket](https://github.com/forwardat/Arduino-Xml)
- [HTMLParser Biblioteket](https://github.com/efeiefei/node-html-parser)
- [Mozilla Developer Network - HTML](https://developer.mozilla.org/en-US/docs/Web/HTML)