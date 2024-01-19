---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & varför?

Att extrahera substrängar innebär att plocka ut en specifik del av en sträng i kod. Det är väldigt användbart vid programmering för att bearbeta och analysera data effektivt.

## Hur man gör:
Extrahera substrängen "hej" från strängen "hej världen" i Arduino är enkelt. Här är kodexempel och utmatningsexempel:

```Arduino
void setup() {  
  Serial.begin(9600);  
}  
void loop() {  
  String str = "hej världen";  
  String subStr = str.substring(0, 3);  
  Serial.println(subStr); 
  delay(1000);
}

```
När du laddar upp och kör denna kod, kommer "hej" att visas i serieterminalen varje sekund.

## Fördjupning:
Extraktion av substrängar har varit en viktig del av programmering sedan starten. I Arduino är den mest populära metoden att använda `substring()`-metoden. Men det finns alternativ som inkluderar att använda pekare och teckenarrayer, även om det kan vara mer komplicerat.

För att `substring()` ska fungera effektivt behöver Arduino minne att lagra substrängen. Det är också viktigt att vara medveten om att indexeringen börjar från noll - det vill säga att första tecknet i strängen är på indexplatsen noll.

Du bör också vara medveten om att `substring()`-metoden skapar en ny sträng och det ökar minnesanvändningen. I projekt där minne är en knapp resurs kan andra metoder behöva övervägas.

## Se också:
För ytterligare information och källor se följande länkar:

1. [Arduino String Objects](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
2. [String Manipulation](http://www.cplusplus.com/reference/string/string/substr/)

Öva och experimentera med koden och du kommer att få grepp om att ta ut substrängar på nolltid. Lycka till!