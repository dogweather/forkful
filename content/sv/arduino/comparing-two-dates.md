---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Jämförelse av två datum är en funktion där man ser om ett datum kommer före, efter eller är samma som ett annat datum. Programmerare använder det för att skapa logik där tidsförlopp är viktigt, som kalenderappar.

## Hur Kan Man:
Här är ett exempel på hur du jämför två datum i Arduino:

```Arduino
// Definiera två datum
int year1 = 2022, month1 = 5, day1 = 20;
int year2 = 2022, month2 = 5, day2 = 21;

if(year1 < year2){ 
  Serial.println("Det första datumet är tidigare");
}
else if (year1 > year2){
  Serial.println("Det andra datumet är tidigare");
}
else { // Samma år
  if(month1 < month2){
     Serial.println("Det första datumet är tidigare");
  }
  else if(month1 > month2){ 
    Serial.println("Det andra datumet är tidigare");
  }
  else { // Samma månad
    if(day1 < day2){
      Serial.println("Det första datumet är tidigare");
    }
    else if(day1 > day2){ 
      Serial.println("Det andra datumet är tidigare");
    }
    else{
      Serial.println("Datumen är samma");
    }
  }
}
```

Utskriften blir: `Det första datumet är tidigare`

## Djupdykning
Arduino är inte känt för sin datumanvändning, då mycket av dess applikationer ligger utanför traditionell dators datahantering. De ovannämnda kodstyckena är enkla och funktionaliteten är inbyggd. Men det finns också bibliotek, till exempel TimeLib, som kan hantera mycket av tids- och dataknepigheterna för dig. 

Det finns många sätt att jämföra datum på. Du kan konvertera båda till UNIX-tidsstämplar och jämföra heltalen, eller så kan du använda specifika bibliotek som kan jämföra datumen för dig.

Detaljer: Arduino lagrar inte datatyperna `date` eller `time` prydligt, så du kan förvänta dig att lagra varje komponent av ett datum (år, månad, dag) som en heltalsvariabel. Jämförelse sker sedan på vanlig sätt med selection (`if`) statements.

## Se Även
Det finns några källor för mer information om jämförelse av datum i Arduino:

1. [Arduino documentation](https://arduino.cc)
2. [TimeLib Library](https://github.com/PaulStoffregen/Time)
3. [A detailed guide on Arduino Time technique](https://www.makerguides.com)