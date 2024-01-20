---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:34:48.616853-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en textsträng innebär att omvandla texten till ett datumformat som datorn förstår. Programmerare gör detta för att enkelt kunna jämföra datum, sortera poster, eller utföra datum- och tidberäkningar.

## How to:
```Bash
#!/bin/bash
datum_str="2023-04-01"
datum=$(date -d "$datum_str" '+%Y-%m-%d')
echo $datum
```
Output:
```
2023-04-01
```
Ett annat exempel, med svensk lokal inställning för att visa dagens namn:
```Bash
#!/bin/bash
export LC_TIME=sv_SE.UTF-8
datum_str="1 april 2023"
datum=$(date -d "$datum_str" '+%A, den %d %B %Y')
echo $datum
```
Output:
```
lördag, den 01 april 2023
```

## Djupdykning
Att tolka datum har varit viktigt sedan datorer började användas. För att hantera datum i `bash` använder vi kommandot `date`. Det är flexibelt och stödjer många format. Det finns alternativ som `dateutils` och externa program som `GNU date`, men i `bash`, är `date` det vanligaste verktyget. Implementeringsdetaljer kan variera mellan olika system – till exempel är `date -d` specifik för GNU date och fungerar inte på macOS, där man istället skulle använda `date -j -f`.

## Se Också
- GNU Coreutils Manual för `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Bash-guide för nybörjare: https://tldp.org/LDP/Bash-Beginners-Guide/html/
- Information om locale settings under Linux: https://www.gnu.org/software/gettext/manual/html_node/Locale-Environment-Variables.html