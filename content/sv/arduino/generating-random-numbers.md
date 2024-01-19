---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Slumpmässiga Nummer med Arduino

## Vad & Varför?

Att generera slumpmässiga nummer innebär att skapa tal som inte kan förutsägas före dess generering. Programmerare använder detta för att skapa osäkerhet i sina program, vilket är särskilt viktigt i spel, simuleringar och tester.

## Såhär Uppnår du detta:

Arduino gör att generera slumpmässiga nummer väldigt enkelt. Använd bara `random()` funktionen.

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int randomNumber = random(1, 101); // generera ett slumpmässigt heltal mellan 1 och 100
  Serial.println(randomNumber);
  delay(1000); // vänta en sekund
}
```

Liksom med andra funktioner kan du anpassa spannet av möjliga utfall. Värdet 101 är inte inkluderat, så detta kommer att returnera ett takl på 1 till 100.

## Djupdykning

Det är dock inte alla slumpmässiga nummer som är skapade lika. I själva verket använder Arduino pseudoslumpmässiga nummergenerering. Dessa "slumpmässiga" nummer följer en förutbestämd sekvens, men för de flesta ändamål är detta tillräckligt bra. 

Alternativt, kan du använda `randomSeed()`-funktionen för att skapa en mer verkligt slumpmässig serie av nummer. Denna funktion använder tidbas indata för att sätta upp en startpunkt för den pseudoslumpmässiga sekvensen.

```Arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0)); // använde en ospecificerad analog ingång för att generera ett slumpmässigt frö
}

void loop() {
  int randomNumber = random(1, 101);
  Serial.println(randomNumber);
  delay(1000); 
}
```

## Se Även

För ytterligare inblick i att generera slumpmässiga nummer kan du besöka följande länkar:

1. [Arduino Random Number Tutorial](https://www.makerguides.com/arduino-random-numbers/)
2. [Arduino Random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
3. [Historical Perspectives on Random Numbers](https://arxiv.org/pdf/0801.4842.pdf)

Lycka till med dina Arduino-projekt!