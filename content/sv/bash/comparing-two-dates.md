---
title:                "Bash: Jämföring av två datum"
simple_title:         "Jämföring av två datum"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en viktig del av Bash programmering, eftersom det gör det möjligt för oss att hantera tidsrelaterade uppgifter. Det kan hjälpa oss att kontrollera olika händelser eller processer baserat på datum och tid, vilket är särskilt användbart i automatiseringsprocesser.

## Hur man gör det

För att jämföra två datum i Bash behöver vi använda oss av `date` kommandot. Detta kommando kan inte bara skapa och visa datum, utan också göra beräkningar och jämföra datum. Se nedan för ett enkelt exempel:

```Bash
date1=$(date -d "2020-04-25" +%s)
date2=$(date -d "2020-04-27" +%s)

if [ $date1 -lt $date2 ]
then
    echo "Datum 1 är innan datum 2"
fi
```

I detta exempel använder vi `date -d` flaggan för att ange ett datum i ett visst format, följt av `%s` för att konvertera datumet till sekunder sedan Unix-epoken (1 januari 1970). Med hjälp av dessa sekunder kan vi sedan jämföra de två datum med hjälp av vanliga villkorsuttryck som `-lt` (mindre än) och `-gt` (större än).

Ett annat användbart verktyg för datumjämförelse är `date +%j`, som returnerar antalet dagar sedan 1 januari. Detta kan användas för att jämföra om två datum ligger på samma dag eller inte.

## Djupdykning

För mer avancerade jämförelser kan man använda `date` kommandot tillsammans med andra kommandon som `cut` och `awk` för att extrahera specifika delar av datumet och jämföra dem med varandra. Till exempel kan man använda `cut` för att extrahera månad, dag och år från två olika datum och sedan jämföra dem med hjälp av `awk`.

## Se även

- [Linuxize: How to Compare Dates in Bash](https://linuxize.com/post/how-to-compare-dates-in-bash/)
- [Cyberciti: Bash Shell: Compare Numbers](https://www.cyberciti.biz/faq/bash-compare-numbers/)