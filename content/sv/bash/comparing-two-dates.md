---
title:                "Bash: Jämföra två datum"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

##Varför: Jämföra två datum i Bash-programmering
Att kunna jämföra två datum är en viktig färdighet inom Bash-programmering när man behöver hantera datum och tid i olika script och program. Det kan vara användbart i många olika situationer, som att kontrollera om en fil är äldre än en annan eller planera automatiska uppdateringar baserat på datum.

##Hur man gör det
För att jämföra två datum i Bash behöver vi först konvertera dem till ett format som kan jämföras, som till exempel Unix-timestamp (antal sekunder sedan 1 januari 1970). Detta kan göras med hjälp av kommandot `date +%s`.

```Bash
date_one="2021-06-01"
date_two="2021-07-01"

#konvertera till Unix-timestamp
date_one_ts=$(date -d "$date_one" +%s)
date_two_ts=$(date -d "$date_two" +%s)

#jämför med hjälp av en if-sats
if [ "$date_one_ts" -lt "$date_two_ts" ]; then
  echo "Date one is earlier than date two."
elif [ "$date_one_ts" -gt "$date_two_ts" ]; then
  echo "Date two is earlier than date one."
else
  echo "The dates are the same."
fi
```

Det här är bara ett grundläggande exempel på hur man kan jämföra två datum i Bash. Det finns många olika sätt att göra det på och det beror ofta på vilken form av datum man har att göra med.

##Djupdykning
För att kunna jämföra datum behöver vi förstå hur de är strukturerade och lagrade. Unix-timestamp är ett vanligt sätt att representera datum inom Bash, men det finns också andra format som till exempel ISO 8601 eller POSIX-tid. Det är viktigt att vara medveten om vilket format ens datum är i för att kunna konvertera till rätt format och jämföra korrekt.

Ett annat sätt att jämföra datum är med kommandot `test` eller `[ ]` som i tidigare exempel. Dessa kommandon kan användas för att jämföra inte bara datum utan också andra variabler, som till exempel strängar eller tal.

##Se även
- [The Linux Command Line: A Complete Introduction](https://linuxcommand.org/tlcl.php)
- [GNU Bash manual: Shell Grammar](https://www.gnu.org/software/bash/manual/html_node/Shell-Grammar.html)
- [Bash-hjälp på stackoverflow](https://stackoverflow.com/questions/tagged/bash)