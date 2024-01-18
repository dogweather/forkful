---
title:                "Att tolka ett datum från en sträng"
html_title:           "Bash: Att tolka ett datum från en sträng"
simple_title:         "Att tolka ett datum från en sträng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att parsra ett datum från en sträng innebär att man extraherar ett datum i ett specifikt format från en textsträng. Detta är ofta nödvändigt för att utföra olika operationer eller för att presentera datumen på ett lämpligt sätt. Programmörer gör detta för att underlätta hanteringen av datum och för att kunna automatisera uppgifter som involverar datum.

## Hur man gör:
Att parsra ett datum från en sträng i Bash kan göras med hjälp av verktyget `date`. Nedan följer ett exempel på hur man kan åstadkomma detta:

```Bash
#!/bin/bash

string_date="2020-04-29"
formatted_date=$(date -d "$string_date" "+%d/%m/%Y")

echo "$formatted_date"
```

Detta kommer att ta en sträng som innehåller datumet "2020-04-29" och konvertera det till formatet "29/04/2020". Detta är ett enkelt exempel på hur man kan parsra ett datum från en sträng, men det finns många fler möjligheter med verktyget `date`. Du kan till exempel ange olika format för output, eller lägga till eller subtrahera datum.

## Djupdykning:
Parsning av datum från strängar är ett viktigt begrepp inom programmering och har funnits med sedan de tidiga dagarna av datorer. Det är inte unikt för Bash, utan det finns många andra språk och verktyg som erbjuder samma funktion. Alternativ till Bash för att parsra datum från strängar inkluderar Python, Java och Perl.

När man parsar datum från strängar bör man vara medveten om att det finns olika format och konventioner för datum runt om i världen. Det är också viktigt att se till att datumen är korrekta och att de parsras på rätt sätt för att undvika problem i programmet.

En mer avancerad metod för att parsra datum från strängar är att använda reguljära uttryck. Detta kan vara användbart om du behöver parsra datum från en text där datumen inte är tydligt definierade eller följer ett specifikt mönster.

## Se även:
Här är några relaterade källor som du kan läsa mer om parsning av datum från strängar i Bash:

- [Linux Bash Handbook](https://linuxhandbook.com/bash/) - en omfattande guide till Bash, inklusive användning av verktyget `date`.
- [The Linux Command Line](http://linuxcommand.org/tlcl.php) - en annan resurs för att lära dig Bash och hitta användbara kommandon.
- [Bash Scripting Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/) - en nybörjarvänlig guide till Bash-scripting som också täcker `date` verktyget.