---
title:                "Beräkning av ett datum i framtiden eller det förflutna"
html_title:           "C++: Beräkning av ett datum i framtiden eller det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Att beräkna ett datum i framtiden eller det förflutna är en vanlig uppgift inom programmering. Det är vanligtvis användbart för att skapa tidsbestämda funktioner eller för att upprätthålla en korrekt tidsstämpel på data.

## Hur man gör det:

För att beräkna ett datum i framtiden eller det förflutna, kan vi använda oss av olika funktioner och datatyper i C++. En vanlig metod är att använda sig av `struct tm` från `time.h` biblioteket. Här är ett exempel på hur man kan skriva en funktion som returnerar ett datum efter ett visst antal dagar efter det nuvarande datumet.
```C++
struct tm addDays(int days){
    time_t now = time(0);
    struct tm curr_date = *localtime(&now);
    struct tm new_date = curr_date;
    new_date.tm_mday = curr_date.tm_mday + days;
    // Hantera fall där månaden eller året behöver justeras
    mktime(&new_date);
    return new_date;
}

int main() {
    int days = 14;
    struct tm future_date = addDays(days);
    cout << "Datumet i framtiden om " << days << " dagar är " << 
    future_date.tm_mon + 1 << "/" << future_date.tm_mday << "/" << 
    future_date.tm_year + 1900;
}
```
Output:
```
Datumet i framtiden om 14 dagar är 1/28/2020
```

## Djupdykning:

Att räkna ut datum i framtiden eller förflutna har varit en viktig del av datorkunskap sedan länge. I äldre system användes oftast ett julianskt kalendersystem, men idag används i stället Gregorianska kalendern som är den vanligaste i världen. Alternativet till att använda `struct tm` är att använda API:er som `std::chrono` och `boost::date_time`. Dessa ger fler funktioner och mer flexibel funktionalitet för att hantera datum och tider.

## Se även:

[https://www.cplusplus.com/](https://www.cplusplus.com/) - Officiell hemsida för C++ som innehåller dokumentation och resurser
[https://www.geeksforgeeks.org/](https://www.geeksforgeeks.org/) - En populär sida för programmeringsuppgifter och artiklar