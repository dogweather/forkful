---
title:    "Arduino: Generering av slumpmässiga nummer"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en nyckelfunktion inom många projekt med Arduino. Det är användbart för allt från spel och lotterier till sensorbaserade experiment och datainsamling.

## Hur man gör det

För att generera ett slumpmässigt nummer i Arduino, behöver du använda dig av en funktion som heter "random()". Denna funktion tar två parametrar - det minsta och det största möjliga värdet för det slumpmässiga numret som ska genereras.

Ett enkelt exempel på hur man använder random() funktionen i en loop för att generera fem slumpmässiga nummer mellan 1 och 10 skulle se ut såhär:

```Arduino
void loop() {
  for (int i = 0; i < 5; i++) {
    int randomNum = random(1, 11); // generera ett slumpmässigt nummer mellan 1 och 10
    Serial.println(randomNum); // skriv ut numret i seriamonitorn
  }
}
```

Här är en möjlig utdata från detta program:

```
8
2
6
9
4
```

Som du kan se, genererar varje loop ett nytt slumpmässigt nummer mellan 1 och 10. Du kan också använda random() funktionen för att generera slumpmässiga nummer i ett visst intervall, till exempel mellan 500 och 1000.

## Djupdykning

För att förbättra kvaliteten på de slumpmässiga numren som genereras, kan du också använda seed() funktionen. Detta ändrar startvärdet för random() funktionen och ger en mer "slumpmässig" sekvens av nummer.

En seed() funktion kan ta ett vilket heltal som helst som parameter, men den vanligaste metoden är att använda analogRead() för att läsa av ett brusigt värde från en analog pinne. Detta säkerställer att startvärdet för random() ändras varje gång programmet körs, vilket ger ännu mer slumpmässiga nummer.

En förbättrad version av det tidigare exemplet som använder seed() ser ut såhär:

```Arduino
int seedVal = analogRead(A0); // läs in ett slumpmässigt värde från pin A0
randomSeed(seedVal); // använd detta värde som seed för random() funktionen

void loop() {
  for (int i = 0; i < 5; i++) {
    int randomNum = random(1, 11); // generera ett slumpmässigt nummer mellan 1 och 10
    Serial.println(randomNum); // skriv ut numret i seriamonitorn
  }
}
```

Här är en möjlig utdata från detta program:

```
3
7
10
1
9
```

Du kan också använda olika typer av icke-linjära matematiska funktioner för att generera mer komplexa slumpmässiga mönster. Du kan till exempel använda en sinuskurva för att få ett mer sinuskurveliknande mönster av de slumpmässiga numren.

## Se även

- [Arduino Random() Funktion](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Seed() Funktionen](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)
- [AnalogRead() Funktionen](https://www.arduino.cc/reference/en/language/functions/analog-io/analogread/)