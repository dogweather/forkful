---
title:                "Een datum omzetten naar een string"
date:                  2024-01-28T21:57:11.487693-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum omzetten naar een string"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
We zetten datums om in strings om ze leesbaar te maken voor mensen of om ze op te slaan en weer te geven. Het gaat erom ruwe datumgegevens te presenteren op een manier die voor ons logisch is.

## Hoe te:
C maakt deze klus vrij eenvoudig met de `strftime` functie. Hier is een snel voorbeeld:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t ruwetijd;
    struct tm * tijdinfo;
    char buffer[80];

    time(&ruwetijd);
    tijdinfo = localtime(&ruwetijd);

    strftime(buffer, sizeof(buffer), "%d-%m-%Y %I:%M:%S", tijdinfo);
    printf("Geformatteerde datum & tijd: %s\n", buffer);

    return 0;
}
```

Voorbeelduitvoer zou kunnen zijn: `Geformatteerde datum & tijd: 22-03-2023 09:45:12`

## Diepgaand:
Historisch gezien heeft de tijdafhandeling in C zijn eigenaardigheden: eerdere normen hadden bijvoorbeeld geen gestandaardiseerde manier om met tijdzones om te gaan. Nu hebben we `strftime` als onderdeel van de Standaard C Bibliotheek vanaf C89, wat ons een consistente manier biedt om `struct tm` tijdstructuren om te zetten in strings, met controle over de opmaak.

Wat betreft alternatieven, men zou handmatig waarden uit `struct tm` kunnen halen en ze aan elkaar koppelen, maar dat is het wiel opnieuw uitvinden. Er is ook de POSIX `strptime` functie, die omgekeerd werkt, van string naar `struct tm`.

Wanneer je `strftime` gebruikt, onthoud: de grootte van de buffer doet ertoe; te klein en je string wordt afgekapt. Ook laten de opmaakspecificaties in `strftime` je toe om de datum en tijd op verschillende mensvriendelijke manieren aan te passen, zoals het wijzigen van locales of de datum-tijdrepresentatie.

## Zie Ook:
- C Standaard Bibliotheek documentatie: https://en.cppreference.com/w/c/chrono/strftime
- GNU C Bibliotheekhandleiding over Tijd: https://www.gnu.org/software/libc/manual/html_node/Time.html
- strftime opmaakspecificaties: https://www.gnu.org/software/libc/manual/html_node/Low_002dLevel-Time-String-Parsing.html#Low_002dLevel-Time-String-Parsing
