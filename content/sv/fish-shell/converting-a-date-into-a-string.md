---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att konvertera ett datum till en sträng i programmering innebär att man ändrar representationen av datumet till textform. Detta gör man för att enklare kunna visa och manipulera datumet i programmet.

## Så här gör du:
Med Fish Shell tog funktionen att skriva ut datum i strängformat precis så här:

```Fish Shell
set datum (date "+%Y-%m-%d")
echo $datum
```
Detta ger tillbaka något i stil med: "2021-09-02"

Ett mer kompakt sätt att skapa datumsträngen:

```Fish Shell
echo (date "+%Y-%m-%d")
```

## Djupdykning
Det historiska sammanhanget för att skriva om datum som strängar kommer ifrån ett behov hos gamla textbaserade gränssnitt att visa datum på ett läsbart sätt. I dagens grafiska gränssnitt finns fortfarande detta behov för att hantera och visa datum i vissa former.

Alternativ till att använda `date` i Fish Shell kan vara att använda `strftime` funktionen i språk som Python eller Java för att konvertera ett datum till en sträng.

För implementeringsdetaljer betraktas datakonvertering som en process att omvandla data från ett format till ett annat. Som nämnts sker datatransformering i Fish Shell enligt följande format `"+%Y-%m-%d"`. Här står "+" för output-beteende, "Y" för fyrsiffrigt år, "m" för tvåsiffrig månad och "d" för tvåsiffrig dag.

## Se Också
Du kan läsa mer om Fish Shell och dess användning på nedan webbplatser:
- Fish Shell official documentation: https://fishshell.com/docs/current/index.html
- Stack Overflow Q&A on Fish Shell: https://stackoverflow.com/questions/tagged/fish
- GitHub repository for Fish Shell: https://github.com/fish-shell/fish-shell