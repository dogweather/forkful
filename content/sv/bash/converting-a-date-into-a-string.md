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

## Vad & Varför?

Att konvertera ett datum till en sträng innebär att ändra datumformattet till textform. Programmerare gör detta för att göra datumen mer läsbara och för att underlätta jämförelser och sorteringar.

## Hur man gör:

Använda datumkommandot. Exempel:

```Bash
datum=$(date +'%Y-%m-%d')
echo $"Datumet är: $datum"
```
Exempelutmatning blir:

```Bash
Datumet är: 2022-08-04
```
Som du ser är datumet nu en sträng som kan användas för många olika ändamål.

## Fördjupning

Bash scriptning, som banade väg för detta, började som en Unix shell och kommandospråk skapad av Brian Fox år 1989. Alternativt kan du använda 'printf' -sättet men 'date' -kommandot är mer direkt och mindre komplicerat. 

När du konverterar ett datum till en sträng i Bash, tänk på att olika system kan ha varierande datum- och tidsformat. Du kanske behöver anpassa kodens exakta format beroende på ditt system och önskat resultat.

## Se också:

- Bash Programming Guide (https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)
- Bash Date Command Usage (https://linuxhandbook.com/date-command/)
- More on Printf in Bash (https://www.cyberciti.biz/faq/understanding-bash-printf-command_hex/)