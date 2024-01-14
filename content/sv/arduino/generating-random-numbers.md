---
title:                "Arduino: Skapa slumpmässiga tal"
programming_language: "Arduino"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga tal är en nyckelkomponent i många Arduino projekt. Detta kan vara användbart för att skapa variation i en loop eller för att simulera naturliga processer. Det kan också användas för att skapa slumpmässiga beslut i olika applikationer.

## Så här gör du

För att generera slumpmässiga tal i Arduino, används funktionen "random(min, max)" där "min" är det lägsta värdet och "max" är det högsta värdet som det slumpmässiga talet kan ha. Nedan följer ett exempel på kod som genererar fem slumpmässiga tal mellan 0 och 10:

```Arduino
void setup() {
    Serial.begin(9600); //startar seriell kommunikation
}

void loop() {
    //genererar fem slumpmässiga tal och skickar dem till den seriella monitor för att visa resultatet
    for (int i = 0; i < 5; i++) {
        int num = random(0, 10) //generera ett slumpmässigt tal mellan 0 och 10
        Serial.print("Slumpmässigt tal: ");
        Serial.println(num); //skriv ut det slumpmässiga talet
    }
    delay(1000); //vänta en sekund innan nästa loop
}
```
Output:
```
Slumpmässigt tal: 8
Slumpmässigt tal: 2
Slumpmässigt tal: 0
Slumpmässigt tal: 7
Slumpmässigt tal: 9
```

## Djupdykning

Arduino använder en pseudoslumpgenerator för att generera slumpmässiga tal. Detta betyder att talen inte är helt slumpmässiga, utan de genereras utifrån ett startvärde. Detta startvärde kallas "seed" på engelska och kan ändras för att få olika sekvenser av slumpmässiga tal.

Om man inte specificerar ett "seed"-värde i koden så används vanligtvis det aktuella millis() värde som startvärde. Detta värde ökar med en millisekund varje gång funktionen körs, vilket betyder att en ny serie av tal genereras varje gång koden startar om.

För att ändra "seed"-värdet och få en annan sekvens av slumpmässiga tal kan du använda funktionen "randomSeed()" tillsammans med ett tal som argument. Till exempel, om du vill få samma sekvens av slumpmässiga tal varje gång koden körs kan du använda millisekundvärdet vid kön/setup, som nedan:

```Arduino
void setup() {
    randomSeed(millis()); //använd aktuellt millisec-värde som "seed"
    //resten av koden
}

void loop() {
    //resten av koden
}
```

Det finns även andra sätt att generera slumpmässiga tal med mer komplexitet, till exempel genom att använda externa kretsar eller sensorer.

## Se även

- [Arduino referens för random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [YouTube-tutorial om att generera slumpmässiga tal i Arduino](https://www.youtube.com/watch?v=z4wHYJxg54g)
- [Projektidéer med användande av slumpmässiga tal i Arduino](https://create.arduino.cc/projecthub?by=part&type=tutorial&difficulty=&category=random-numbers&part_id=8232&sort=trending)