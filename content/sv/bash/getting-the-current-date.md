---
title:                "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att hämta den aktuella datumen är en viktig del av programmering i Bash. Det låter dig visa den aktuella tiden och datumet i ett läsbart format och används ofta för att skapa dynamiska filnamn eller som en del av en automatisk backupprocess.

## Hur man gör det

För att hämta den aktuella datumen i Bash använder vi kommandot `date`. Detta kommando visar standardtiden och datumet om inga argument anges, men kan också formateras för att visa endast önskad information.

```Bash
date
```

Detta ger utskrift som följande:

```Bash
Sun Jul 26 11:30:55 CET 2020
```

Om vi vill ha ett specifikt datumformat kan vi använda flaggan `+` följt av önskat format.

```Bash
date +"%Y-%m-%d"
```

Detta skulle ge oss utskrift i formatet "åååå-mm-dd", exempelvis `2020-07-26`.

Vi kan också kombinera olika flaggor för att få ett mer specifikt format. Till exempel, för att få utskrift i formatet "hh:mm:ss" (timmar:minuter:sekunder) kan vi använda följande kommando:

```Bash
date +"%H:%M:%S"
```

## Djupdykning

När du använder `date`-kommandot finns det många olika flaggor och formatalternativ att utforska. En av de vanligaste flaggorna är `-d`, som låter dig ange ett specifikt datum och tid, istället för den aktuella. Till exempel kan du använda detta för att få utskrift av en viss tidpunkt i framtiden:

```Bash
date -d "1 hour"
```

Detta skulle ge dig utskrift av tiden 1 timme framåt från den aktuella tiden.

En annan användbar flagga är `-u`, som ger utskrift i UTC-tid istället för lokaltid. Detta kan vara användbart när du behöver synkronisera filer eller data mellan olika tidszoner.

## Se närings

Se även:

* [Bash Documentation - Date Command](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)
* [Linuxize - Bash Date Command](https://linuxize.com/post/bash-date-command/)
* [GeeksforGeeks - Bash Date Command](https://www.geeksforgeeks.org/date-command-in-linux-with-examples/)