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

Att omvandla ett datum till en sträng handlar om att konvertera ett datum, som till exempel 12 maj 2021, till en textsträng som ser ut som "12-05-2021". Programmerare gör detta för att kunna använda datumet i koden på ett enklare sätt, till exempel för att jämföra med andra datum eller skapa en titel för en fil baserad på datumet.

## Såhär gör man:

För att konvertera ett datum till en sträng i Bash kan vi använda "date" kommandot tillsammans med formatflaggan. Här är ett exempel på hur man kan konvertera dagens datum till en sträng med formatet "dd-mm-yyyy":

```Bash
# Spara datumet i en variabel
today=$(date +"%d-%m-%Y")

# Visa resultatet
echo "Dagens datum är $today"
# Output: Dagens datum är 12-05-2021
```

Vi kan också använda andra formatflaggor för att skapa en annan typ av sträng. Till exempel skulle kommandot ```date +"%A, %d %B %Y"``` ge oss en sträng som "onsdag, 12 maj 2021". Det finns många olika formatflaggor att välja mellan beroende på vilken typ av sträng vi vill ha.

## Djupdykning:

Att konvertera ett datum till en sträng är ett vanligt problem inom programmering och det finns många olika sätt att göra det på. Bash är bara ett av de verktyg som kan användas för att lösa detta problem, men det är enkelt och lättviktigt att använda för enklare uppgifter.

En alternativ metod för att konvertera datum till strängar är att använda "awk" kommandot tillsammans med "strftime" funktionen. Detta kan vara användbart om du behöver göra mer komplicerade omvandlingar eller om du arbetar med stora mängder data. Men för enkla uppgifter, som att bara få dagens datum i ett visst format, fungerar Bash's "date" kommando väl.

## Se även:

- [Bash:s officiella dokumentation för "date" kommandot](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [En handledning om hur man använder "awk" och "strftime"](https://www.computerhope.com/unix/awk-strftime.htm)