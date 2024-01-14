---
title:                "C: Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Att kunna beräkna ett datum i framtiden eller förflutna är en användbar funktion inom programmering. Det kan hjälpa till att organisera och planera viktiga händelser eller samla data för analyser. 

## Så här
Att beräkna ett datum i C-programmering kan göras med hjälp av olika funktioner och metoder, beroende på vad som passar bäst för ditt specifika projekt. Nedan finns några kodexempel för att illustrera hur man kan gå tillväga.

```C
// Beräkna datumet från imorgon
int main() {
    int day = 28; // Dag 28
    int month = 12; // December
    int year = 2020; // År 2020
    int numDays = 1; // Antal dagar att lägga till

    // Anropa funktionen för att beräkna datumet
    calcFutureDate(day, month, year, numDays);
    
    return 0;
}

// Funktionen för att beräkna ett datum i framtiden
void calcFutureDate(int day, int month, int year, int numDays) {
    // Uppdatera dag och månad beroende på antalet dagar som ska läggas till
    day = day + numDays;
    if(day > daysInMonth(month, year)) {
        day = day - daysInMonth(month, year);
        month = month + 1;
    }

    // Uppdatera året om det behövs
    if(month > 12) {
        year = year + 1;
        month = 1;
    }

    // Skriv ut datumet i önskad format
    printf("%d/%d/%d", day, month, year);
}

// Funktionen som räknar ut antalet dagar i en månad
int daysInMonth(int month, int year) {
    int numDays;
    // Kontrollera om det är ett skottår
    if(month == 2 && isLeapYear(year)) {
        numDays = 29;
    }
    // Annars, använd en switch-sats för att bestämma antalet dagar i månaden
    else {
        switch(month) {
            case 1: numDays = 31; break;
            case 2: numDays = 28; break;
            case 3: numDays = 31; break;
            case 4: numDays = 30; break;
            case 5: numDays = 31; break;
            case 6: numDays = 30; break;
            case 7: numDays = 31; break;
            case 8: numDays = 31; break;
            case 9: numDays = 30; break;
            case 10: numDays = 31; break;
            case 11: numDays = 30; break;
            case 12: numDays = 31; break;
        }
    }

    return numDays;
}

// Funktionen som avgör om ett år är ett skottår
int isLeapYear(int year) {
    if(year % 400 == 0) return 1;
    if(year % 100 == 0) return 0;
    if(year % 4 == 0) return 1;

    return 0;
}
```

Detta är en enkel metod för att beräkna ett datum i framtiden. Det finns många andra funktioner och metoder som kan användas för att göra mer komplexa beräkningar, såsom att ta hänsyn till olika datumformat eller hantera negativa antal dagar för att beräkna ett datum i förflutna. 

## Fördjupning
För den som är intresserad av att lära sig mer om hur datumberäkningar fungerar i C-programmering finns det många resurser på nätet som ger en djupare förståelse för olika tekniker och metoder. Det är också bra att utforska olika kodexempel och experimentera med dem för att få en bättre känsla för hur man kan anpassa dem efter sina egna behov.

## Se även
- [C-programmering för nybörjare](https://www.programiz.com/c-programming)
- [Datum och tidshantering i C](https://www.geeksforgeeks.org/date-time-functions-in-c