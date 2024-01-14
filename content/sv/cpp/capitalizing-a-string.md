---
title:                "C++: Kapitalisera en sträng"
simple_title:         "Kapitalisera en sträng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

I denna bloggpost kommer vi att titta på hur man kan förkapa en sträng i C ++. Det är en vanlig uppgift som utvecklare stöter på när de arbetar med textdata. Att förkapa en sträng innebär att göra det första bokstaven i varje ord i strängen till en stor bokstav. Detta kan vara användbart när man vill formatera data eller när man vill ha en enhetlig presentation av information.

## Hur man gör

För att förkapa en sträng i C ++ måste vi först inkludera standardbiblioteket `string` som ger oss tillgång till funktioner för strängmanipulering. Sedan behöver vi en variabel som innehåller den sträng som vi vill förkapa. Vi kan använda funktionen `toupper()` för att göra om den första bokstaven i strängen till en stor bokstav. Sedan kan vi använda en lökloop för att gå igenom alla bokstäver i strängen och förkapa dem. 

Här är en kodexempel som visar detta steg för steg:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  // Här skapar vi en variabel som innehåller den sträng som vi vill förkapa
  string text = "hej på dig";

  // Använder toupper() för att göra första bokstaven stor
  text[0] = toupper(text[0]);
  
  // Löploop som förkapar alla bokstäver i strängen
  for (int i = 0; i < text.length(); i++) {
    if (text[i] == ' ') {
      text[i+1] = toupper(text[i+1]);
    }
  }

  // Skriver ut den förkaptulerade strängen
  cout << text;

  return 0;
}
```

Output: `Hej På Dig`

## Deep Dive

För att förkapa en sträng i C ++ måste vi förstå hur strängar och tecken fungerar i programmering. En sträng är en sekvens av tecken som representerar en text. Varje tecken är egentligen en numerisk kod från ASCII-tabellen som berättar för datorn hur det ska tolkas. I vår kodexempel ovan använder vi funktionen `toupper()` för att ändra den numeriska koden för bokstaven till dess motsvarande stor bokstav.

För att först skapa en sträng i C ++ använder vi datastrukturen `string` som är en del av standardbiblioteket. `string`-typen har en mängd olika funktioner som vi kan använda för att manipulera strängar, inklusive `toupper()`.

En viktig sak att tänka på när vi arbetar med strängar i C ++ är att en sträng är en array av tecken. Detta betyder att varje tecken har en plats i minnet och vi kan komma åt enskilda tecken genom att ange deras position i strängen.

## Se även

- [C ++ - strängdatastruktur](https://www.w3schools.com/cpp/cpp_strings.asp)
- [ASCII-teckenkoder](https://www.ascii-code.com/)