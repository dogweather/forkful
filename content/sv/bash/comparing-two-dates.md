---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jämföra två datum handlar om att avgöra vilket datum som inträffar först. Programmerare gör detta för att hantera tid och datum på ett korrekt och effektivt sätt, till exempel för att spåra ändringar och deadlines.

## Hur man gör:
För att jämföra två datum i Bash kan vi använda `-d` flaggan i kombination med `date` kommandot. Låt oss jämföra dagens datum med "2022-12-31" till exempel.

```Bash
TODAY=$(date +%Y-%m-%d)
END_DATE="2022-12-31"

if [[ "$(date -d $TODAY +%s)" -gt "$(date -d $END_DATE +%s)" ]]; then
   echo "Today is after the end date."
else
   echo "Today is before the end date."
fi
```
Kodens output kommer att vara antingen "Today is after the end date." or "Today is before the end date." beroende på dagens datum.

## Fördjupning
Bash, avsedda för Unix shell och kommandospråk, skrevs ursprungligen av Brian Fox och släpptes 1989. Att jämföra datum i Bash kan göras på flera sätt. I detta exempel använde vi `date` kommandot och `-d` flaggan för att konvertera stringen till epochtid (antalet sekunder sedan 1970-01-01 00:00:00 UTC) och utförde sedan jämförelsen.

Det finns alternativ till Bash för att jämföra datum, till exempel Python och Perl, vilka är mer lämpade för komplexa datumjämförelser, som datum aritmetik. Dock, för några enkla jämförelser, Bash räcker till.

De verktyg vi använde här, `date` och `-d`, finns tillgängliga i de flesta Unix-liknande operativsystem. Men notera att deras beteende kan variera något beroende på systemkonfiguration och lokala tidszoninställningar.

## Se även
För ytterligare läsning och lärande, ta en titt på följande användbara källor:
- [Bash manpage](https://man7.org/linux/man-pages/man1/bash.1.html)
- [GNU Date Manual](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Forumsdiskussioner om Bash Date Jämförelser](https://stackoverflow.com/questions/3430181/bash-date-compare)