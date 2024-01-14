---
title:                "Arduino: Hämta aktuellt datum"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att få reda på den aktuella datumet kan vara en användbar funktion i många projekt. Den kan hjälpa till att spåra händelser, schemalägga aktiviteter eller helt enkelt ge en allmän uppfattning om tiden.

## Hur man gör det
För att få den aktuella datumet i Arduino behöver du använda `millis()` funktionen. Detta returnerar antalet millisekunder sedan enheten startade. Genom att konvertera detta till en läsbar form kan vi få datumet.

```Arduino
unsigned long nu = millis(); //Spara aktuellt antal millisekunder i en variabel
unsigned long dagar = nu / (1000*60*60*24); //Konvertera till antal dagar
Serial.println(dagar); //Skriv ut antal dagar sedan enheten startade
```

Output:
```Arduino
18558
```

Detta nummer motsvarar antalet dagar sedan enheten startade. För att få ett mer läsbart datum kan man använda några matematiska beräkningar och `sprintf()` funktionen.

```Arduino
unsigned long nu = millis(); //Spara aktuellt antal millisekunder i en variabel
unsigned long dagar = nu / (1000*60*60*24); //Konvertera till antal dagar
int år = 1970 + dagar / 365; //Hitta år genom att dela antalet dagar med antalet dagar på ett år
int dag = dagar % 365; //Hitta dag genom att använda modulo-operatorn för att få återstående dagar
sprintf("%d/%d/%d", dag, år, dag); //Formatera datumet och skriv ut det
```

Output:
```Arduino
27/2025/12 //Om enheten startade den 1 januari 1970
```

## Djupdykning
Som du kanske märkte så är denna metod för att få den aktuella datumet inte perfekt. Trots att enheten har gått i flera år så visar datumet endast 10 år (i detta fall). Detta beror på att `millis()` funktionen endast returnerar ett 32-bitars heltal vilket motsvarar cirka 50 dagar på en Arduino som går på 16 MHz. För att få ett mer exakt datum kan man använda en realtidsklocka eller en extern RTC-modul.

## Se även
- [Användning av millis() funktionen](https://www.arduino.cc/reference/sv/language/functions/time/millis/)
- [Skriva omvandlingsfunktioner för millis() till dage/datum](https://www.arduino.cc/reference/sv/language/functions/time/millis/)
- [Användning av realtidsklocka (RTC) med Arduino](https://www.arduino.cc/en/Tutorial/RTClib)