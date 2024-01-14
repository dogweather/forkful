---
title:                "Bash: Konvertera ett datum till en sträng"
simple_title:         "Konvertera ett datum till en sträng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera ett datum till en sträng är en vanlig uppgift inom Bash-programmering. Det kan vara användbart när man vill lista filer efter datum eller jämföra datum mellan olika filer.

## Så här gör du
För att konvertera ett datum till en sträng i Bash, används kommandot `date` tillsammans med önskad formatsträng. Här är ett exempel på hur man konverterar dagens datum till en sträng i formatet ååååmmdd:

```Bash
date +"%Y%m%d"
```

Output: `20210316` (för 16 mars 2021)

Det finns många olika formatsträngar som kan användas för att få önskad utformat på datumet. Här är några exempel:

- `%Y` för året i fyrsiffrigt format
- `%m` för månad i tvåsiffrigt format
- `%d` för dag i tvåsiffrigt format
- `%H` för timme i 24-timmarsformat
- `%M` för minut i tvåsiffrigt format
- `%S` för sekund i tvåsiffrigt format

## Djupdykning
Kommandot `date` har många fler möjliga formatsträngar än de vi nämnde i avsnittet ovan. En komplett lista och beskrivning av dessa finns i Bash-manualen. Det är även möjligt att kombinera flera formatsträngar för att få en mer exakt utformat på datumet.

En annan användbar funktion är möjligheten att konvertera en specifik datum och tid till en sträng. Detta görs genom att ange datumet efter flaggan `-d`. Här är ett exempel på hur man konverterar 25 december 2021 kl. 15:30 till en sträng i formatet ååååmmddHHMM:

```Bash
date -d "2021-12-25 15:30" +"%Y%m%d%H%M"
```

Output: `202112251530`

## Se även
- [Bash Reference Manual - date](https://www.gnu.org/software/bash/manual/html_node/Bash-Reference-Manual.html#Commands-For-Dates-and-Times)
- [How to Convert Dates to Strings in Bash](https://linuxize.com/post/how-to-convert-dates-to-strings-in-bash/)