---
title:                "Arduino: Jämföra två datum"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara användbart i många olika situationer. Det kan till exempel användas för att beräkna hur många dagar det är mellan två specifika datum eller för att kontrollera om ett visst datum har passerat.

## Så här gör du

För att jämföra två datum i Arduino kan du använda funktionen `compare()` som finns tillgänglig i Arduino Date and Time Library. Den tar in två argument, vilka ska vara av typen `DateTime` och jämför dem med varandra. Om det första datumet är tidigare än det andra kommer funktionen att returnera ett negativt tal, om de är lika kommer den att returnera 0 och om det första datumet är senare än det andra kommer den att returnera ett positivt tal.

Ett exempel på hur du kan använda `compare()`-funktionen i en kod är:

```Arduino
#include <DateTime.h>

void setup() {
  DateTime date1(2021, 9, 18); // första datumet
  DateTime date2(2021, 9, 25); // andra datumet
  int comparison = compare(date1, date2);
  Serial.println(comparison); // utskrift: -7
}

void loop() {

}
```

I detta exempel jämförs hur många dagar det är mellan de två datumen, och eftersom `date1` är tidigare än `date2` returneras ett negativt tal som visar skillnaden mellan dem.

## Djupdykning

När du jämför datum i Arduino är det viktigt att hålla koll på vilken tidzon du befinner dig i, eftersom datumet kan skilja sig beroende på detta. Det finns också olika formateringar av datum, såsom månad/dag/år eller dag/månad/år, vilket kan påverka resultatet av jämförelsen.

Det är också värt att notera att Arduino Date and Time Library endast kan hantera datum från år 2000 och framåt. Om du behöver hantera datum före 2000 kan du behöva använda en annan bibliotek.

## Se även

Här är några användbara länkar för mer information om att jämföra datum i Arduino:

- [Referens för `compare()`-funktionen](https://github.com/PaulStoffregen/Time/blob/master/DateTime.h)
- [Dokumentation för Arduino Date and Time Library](https://github.com/PaulStoffregen/Time)
- [Tutorial om datum och tid i Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/DateTime)