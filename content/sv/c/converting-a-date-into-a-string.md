---
title:                "Omvandla ett datum till en sträng"
html_title:           "C: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Att omvandla ett datum till en sträng är en vanlig uppgift inom programmering. Det innebär att man tar ett datum i ett visst format, som till exempel åååå-mm-dd, och gör om det till en sekvens av bokstäver, siffror och specialtecken. Detta är nödvändigt när man vill visa upp datumet för användaren eller spara det i en databas.

# Hur man gör:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Skapa ett datum-objekt
    time_t date;
    // Tilldela ett datum (2019-10-18)
    date = mktime(2019, 10, 18, 0, 0, 0);
    // Skapa en sträng som ska hålla datumet
    char date_string[11];
    // Använd funktionen 'strftime' för att konvertera datumet till en sträng
    strftime(date_string, 11, "%Y-%m-%d", localtime(&date));
    // Skriv ut strängen
    printf("Datumet är: %s\n", date_string);
    return 0;
}
```
Output: Datumet är: 2019-10-18 

I exemplet ovanför använder vi funktionen ```mktime``` för att skapa ett datum-objekt och tilldela det ett datum. Sedan använder vi funktionen ```strftime``` för att konvertera datumet till en sträng enligt det angivna formatet. Det resulterande strängen kan sedan användas för att visa datumet för användaren eller spara det i en databas.

# Djupdykning:

Historisk kontext: Konvertering av datum till strängar har sina rötter i Unix-operativsystemet. Eftersom datum lagras som en sekundräknare sedan 1970, är det nödvändigt att konvertera det till ett läsbart format för att användas av människor.

Alternativ: Det finns flera olika funktioner som kan användas för att konvertera datum till strängar, som till exempel ```ctime``` och ```strptime```. Det finns också externa bibliotek som kan användas för mer avancerade datumsformat.

Implementation detaljer: Konversionen av datum till strängar innebär att man tar bort den inre representationen av datumet och omvandlar det till en läsbar sekvens av tecken. Detta kan göras med hjälp av olika kommandon och teckenkoder, beroende på det önskade formatet.

# Se även:

- Dokumentation för funktionen ```strftime``` i C
- Mer om tids- och datumhantering i C på StackOverflow