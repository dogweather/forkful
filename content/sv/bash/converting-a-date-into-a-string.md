---
title:                "Omvandla ett datum till en sträng"
html_title:           "Bash: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna konvertera ett datum till en sträng är en användbar färdighet när man arbetar med Bash-scripting. Det kan hjälpa till att automatisera uppgifter som kräver specifika datumformat och göra dem mer läsbara för användaren.

## Hur man gör det
För att konvertera ett datum till en sträng i Bash kan man använda kommandot "date". Det finns många olika sätt att ange ett datumformat, men vi kommer att fokusera på det vanligaste formatet, "YYYY-MM-DD". Här är ett exempel på hur man skulle göra det:

```Bash
date +%Y-%m-%d
```

Detta kommando kommer att returnera dagens datum i formatet YYYY-MM-DD. Om du till exempel kör det idag (23 oktober 2021) kommer det att se ut så här:

```Bash
2021-10-23
```

Du kan också använda andra flaggor för att anpassa datumet till det format du behöver. Till exempel, om du vill inkludera tiden, kan du använda "date +%Y-%m-%d %H:%M:%S" som kommer att returnera datumet och tiden i formatet YYYY-MM-DD HH:MM:SS.

## Djupdykning
För att förstå hur ovanstående kommando fungerar behöver man ha grundläggande kunskap om variabler i Bash. "date" kommandot tilldelar värdet av datumet till en variabel genom användning av "%Y" för år, "%m" för månad och "%d" för dag. Detta resulterar i en väggssträng som representerar dagens datum.

## Se även
- [Bash Date Command](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
- [Introduction to Bash Scripting](https://linuxhint.com/bash_scripting_tutorial_beginners/)