---
title:                "Arduino: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Varför

Ibland när du skriver kod på Arduino, kanske du märker att du behöver ändra eller byta ut en del av texten. Det kan vara ett tidsödande och tråkigt arbete att göra detta manuellt, men som tur är finns det en enklare lösning - söka och ersätta funktionen. Genom att använda denna funktion kan du spara tid och undvika felaktigheter i koden.

# Hur man gör

För att använda sök-och-ersätt-funktionen i Arduino, följ dessa steg:

1. Öppna koden du vill ändra i Arduino IDE.
2. Klicka på **Redigera** i menyn och välj **Sök och ersätt**.
3. I rutan som visas kan du söka efter den text du vill ersätta och ange den nya texten du vill ersätta den med.
4. Klicka på **Nästa** för att söka efter nästa förekomst av den ursprungliga texten.
5. Om du vill ersätta denna förekomst, klicka på **Ersätt** eller **Alla** för att ersätta alla förekomster av texten.

Här är ett exempel på hur en sök-och-ersätt-operation kan se ut i Arduino IDE:

```Arduino
void setup() {
  //Här söker vi efter "lcd.init();" och ersätter det med "lcd.begin();"
  lcd.init();
  lcd.init();
  lcd.begin();
}

void loop() {
 //Vi kan också ersätta texten i kommentarer
 //Vi söker efter "loop" och ersätter det med "loopForever"
  loop();
  loop();
  loopForever();
}
```

Resultatet av koden ovan skulle vara:

```Arduino
void setup() {
  lcd.begin();
  lcd.begin();
  lcd.begin();
}

void loop() {
  loopForever();
  loopForever();
  loopForever();
}
```

# Djupdykning

Det finns många olika sätt att använda sök-och-ersätt-funktionen på i Arduino IDE. Du kan till exempel använda reguljära uttryck för att söka efter mönster istället för en specifik textsträng, eller använda sök-och-ersätt-funktionen i kombination med andra funktioner som **Upprepa** för att göra flera sök-och-ersätt-operationer i en kodfil.

Det är också viktigt att komma ihåg att sök-och-ersätt-funktionen är skiftlägeskänslig, vilket innebär att den kommer att söka efter exakta matchningar av texten du anger. Så om du till exempel vill ersätta "LED" med "led", måste du klicka på **Ersätt alla** för att ändra alla förekomster.

# Se även

- [Arduino IDE - Sök och ersätt](https://www.arduino.cc/reference/en/software/searchandreplace/)
- [Regular Expression Tutorial](https://www.regular-expressions.info/index.html)
- [Upprepa-funktionen i Arduino](https://www.arduino.cc/reference/en/language/structure/control-structure/repeat/)