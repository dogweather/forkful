---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Omvandla ett datum till sträng i C

## Vad och varför?
Att omvandla ett datum till en sträng innebär att du ändrar datumets format så att det kan manipuleras som en textsträng. Detta behövs ofta för att göra datum lättare att arbeta med i olika programmeringssituationer, som att skriva ut datumet eller bearbeta det som en del av en filnamnstruktur.

## Så här gör du:
Att omvandla ett datum till en sträng kan vara rakt på sak. Här är ett exempel på hur du kan göra det med hjälp av `strftime` funktionen i C:

```C
#include <stdio.h>
#include <time.h>

int main() {
    char buff[20];
    time_t nu = time(0);
    struct tm nu_time = *localtime(&nu);

    strftime(buff, sizeof(buff), "%Y-%m-%d", &nu_time);
    printf("Datum som sträng: %s", buff);

    return 0;
}
```

När du kör den här koden, kommer du att få ett utdata som liknar följande:

```C
Datum som sträng: 2022-07-20
```

## Djupdykning
Funktionen `strftime` används här för att formatera tid och datum. Denna funktion härstammar från Unix-världen och har blivit standard i C-språket. Det finns alternativ till `strftime`, som `asctime` eller `ctime`, men de är inte lika flexibla när det gäller formateringsalternativ.

Omvandling av datum till textsträngar ger oss möjlighet att använda datumvärden på ett mer mångsidigt sätt. Utöver att skriva ut dem, tillåter det oss att enkelt infoga datum och tidstämplar i filnamn, loggmeldelser och mycket mer.

## Se även:
För mer information kolla in dessa länkar:
- C Library - <ctime>: http://www.cplusplus.com/reference/ctime/
- Funktionen strftime: https://en.cppreference.com/w/c/chrono/strftime
- C programmeringstid och datum: https://www.learn-cocoa.org/en/dates-and-times-in-c/