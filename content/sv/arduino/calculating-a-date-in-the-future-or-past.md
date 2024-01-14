---
title:    "Arduino: Beräkning av ett datum i framtiden eller i det förflutna"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Varför
Att kunna beräkna ett datum i framtiden eller det förflutna kan vara användbart för många olika ändamål, såsom att skapa en almanacka för ett projekt eller planera en resa. Med hjälp av Arduino kan du enkelt programmera en enhet för att utföra dessa beräkningar åt dig.

# Hur man gör
För att beräkna ett datum i framtiden eller det förflutna behöver du veta det aktuella datumet och hur många dagar du vill lägga till eller dra av från det. Först måste du hämta det aktuella datumet med hjälp av funktionerna `day()`, `month()` och `year()` och lagra dem i variabler. Sedan kan du använda funktionen `addDay()` eller `subtractDay()` för att lägga till eller dra av de önskade antalet dagar. Se nedan för ett exempel:

```Arduino
int currentDay = day();
int currentMonth = month();
int currentYear = year();

int futureDate = addDay(currentDay, currentMonth, currentYear, 10); // 10 är antalet dagar som ska läggas till

Serial.println("Det aktuella datumet är: " + String(currentDay) + "/" + String(currentMonth) + "/" + String(currentYear));
Serial.println("Datumet 10 dagar framåt är: " + String(futureDate) + "/" + String(currentMonth) + "/" + String(currentYear));
```
Output:
```
Det aktuella datumet är: 11/2/2021
Datumet 10 dagar framåt är: 21/2/2021
```

# Djupdykning
Om du vill beräkna ett datum som ligger längre fram eller längre bak i tiden behöver du tänka på eventuella skottår. För att göra detta kan du använda funktionen `leapYear()` för att kontrollera om det aktuella året är ett skottår. Om så är fallet måste du till exempel lägga till ett extra dag i februari. Det är också viktigt att vara medveten om att månader med olika antal dagar kan ge olika resultat. Till exempel kommer att lägga till 30 dagar inte alltid ge ett slutdatum en månad senare.

# Se även
- [arduino.cc](https://www.arduino.cc/)
- [Arduino Reference](https://www.arduino.cc/reference/sv/)