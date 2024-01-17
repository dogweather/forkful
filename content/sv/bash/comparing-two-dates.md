---
title:                "Jämföra två datum"
html_title:           "Bash: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Jämföra två datum är när man kontrollerar om ett datum är tidigare, senare eller lika med ett annat datum. Programmörtar gör detta för att automatisera uppgifter som är beroende av olika datum, t.ex. att skapa en kalender eller schemalägga uppgifter.

## Så här gör du:
Kolla om två datum är lika:
```Bash
date1="2020-08-10"
date2="2020-08-10"
if [ "$date1" == "$date2" ]; then
  echo "Datumen är lika!"
fi
```
Output:
```
Datumen är lika!
```

Kolla om ett datum är tidigare än ett annat:
```Bash
date1="2020-08-10"
date2="2020-08-11"
if [[ "$date1" < "$date2" ]]; then
  echo "Datumet $date1 är tidigare än $date2"
fi
```
Output:
```
Datumet 2020-08-10 är tidigare än 2020-08-11
```

Kolla om ett datum är senare än ett annat:
```Bash
date1="2020-08-10"
date2="2020-08-11"
if [[ "$date1" > "$date2" ]]; then
  echo "Datumet $date1 är senare än $date2"
fi
```
Output:
```
Ingen output eftersom villkoret inte är uppfyllt.
```

## Djupdykning:
Att jämföra datum kan vara viktigt inom programmering eftersom många uppgifter är beroende av datum, t.ex. för att säkerställa att en viss uppgift utförs på rätt dag eller för att beräkna ålder.

Det finns också andra sätt att jämföra datum inom Bash, t.ex. med kommandot "diff" som kan visa skillnaden mellan två datum. Det är också möjligt att konvertera datum till andra format för enklare jämförelse.

Implementeringen av jämförelsen beror på vilket format som används för datum, t.ex. kan datum skrivas som "åååå-mm-dd" eller "dd/mm/åååå". Det är viktigt att använda samma format för att få korrekta resultat.

## Se även:
- Kommandot "diff" för att jämföra skillnaden mellan två filer eller kataloger: https://www.gnu.org/software/diffutils/
- Konvertera datum till andra format med "date" kommandot: https://www.gnu.org/software/coreutils/date
- Mer om datum i Bash: https://www.dostips.com/DtTipsDateTime.php