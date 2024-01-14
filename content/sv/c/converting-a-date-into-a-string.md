---
title:                "C: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna konvertera en datum till en sträng är en viktig färdighet inom C-programmering. Detta gör det möjligt för utvecklare att presentera datum i ett läsbart format för användare, spara datum som strängar i databaser och hantera datumbaserade funktioner på ett effektivt sätt.

## Hur man gör det

För att konvertera ett datum till en sträng, används funktionen strftime(). Denna funktion tar tre parametrar: en buffert för att lagra den resulterande strängen, storleken på bufferten och ett format för hur datumet ska visas. Här är ett exempel som visar hur man kan konvertera datumen i olika format:

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t t = time(NULL);
  struct tm tm = *localtime(&t);

  char buff[100];
  strftime(buff, sizeof(buff), "%d/%m/%Y", &tm);
  printf("Datum som dd/mm/yyyy format: %s\n", buff);

  strftime(buff, sizeof(buff), "%A, %d %B %Y", &tm);
  printf("Datum som veckodag, dd månad yyyy format: %s\n", buff);
  
  strftime(buff, sizeof(buff), "%I:%M %p", &tm);
  printf("Klockslag i timme:minut am/pm format: %s\n", buff);

  return 0;
}
```

Output:

```
Datum som dd/mm/yyyy format: 31/08/2021
Datum som veckodag, dd månad yyyy format: Tuesday, 31 August 2021
Klockslag i timme:minut am/pm format: 12:48 PM
```

## Djupdykning

Med hjälp av strftime() funktionen kan man skapa nästan obegränsade varianter av datumformat. Det finns olika konverteringsspecifikationer som kan användas för att utforma datumet enligt önskemål.

Några av de vanligaste specifikationerna är:

- %d - dag i månaden (01-31)
- %m - månad i året (01-12)
- %Y - år i fyra siffror (2021)
- %B - fullständig månadsnamn (augusti)
- %A - fullständigt veckodagsnamn (tisdag)
- %I - timme på 12-timmarsformat (01-12)
- %p - "am" eller "pm" beroende på tiden
- %M - minut i timmen (00-59)

Det är viktigt att notera att konverteringsformaten kan variera beroende på operativsystem. Det är alltid bra att konsultera dokumentationen för strftime() för att se vilka format som stöds på din specifika plattform.

## Se även

- [Dokumentation för strftime() funktionen](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Beskrivning av konverteringsformat](https://www.cplusplus.com/reference/ctime/strftime/)