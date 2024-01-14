---
title:    "Arduino: Att få den aktuella datumen"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Varför

Att få den nuvarande datumet är en viktig funktion för många projekt som involverar en realtidsklocka. Oavsett om du vill övervaka en tidsintervall, spåra när ett visst evenemang inträffade eller bara hålla koll på vilken dag det är, kan du enkelt få tillgång till den aktuella datumen med hjälp av Arduino. I denna blogginlägg kommer vi att utforska hur du kan göra det och människors olika tillämpningar för att få den nuvarande datumet.

# Hur

För att få den nuvarande datumet i Arduino kan vi använda oss av den inbyggda funktionen "now ()" från Time biblioteket. Detta ger oss en Unix-tidsstämpel som vi sedan kan omvandla till ett mer förståeligt datum-format.

```Arduino
#include <Time.h> // Lägg till Time biblioteket

void setup() {
  Serial.begin(9600); // Anslut till seriel överföring
  setTime(10, 8, 0, 1, 1, 2021); // Ange ett arbiträrt tid och datum
}

void loop() {
  time_t t = now(); // Spara Unix-tidsstämpeln i en variabel
  Serial.println(date(t)); // Skriv ut den nuvarande datumet i formatet "dag/månad/år"
  delay(1000); // Vänta en sekund
}
```

I det här exemplet har vi angett ett arbiträrt tid och datum för att se hur funktionen fungerar. Men du kan ansluta en realtidsklocka till ditt Arduino-board för att få den riktiga tiden och datumet.

# Djupdykning

Den nuvarande datumet är baserad på en Unix-tidsstämpel, vilket är antalet sekunder som har gått sedan 1 januari 1970. Denna tidsstämpel används ofta i programmering för att beräkna datum och tid. Genom att omvandla Unix-tidsstämpeln till ett datum-format kan vi lätt läsa och använda den aktuella datumen i våra projekt. Detta är särskilt användbart om du behöver övervaka flera dagar eller år.

# Se även

- Time library documentation: https://www.arduino.cc/en/Tutorial/Time
- Tutorial on using a real-time clock with Arduino: https://www.instructables.com/Arduino-and-RTC-With-DS3231/
- Useful tips for working with dates in Arduino: https://randomnerdtutorials.com/working-with-time-and-dates-in-arduino/