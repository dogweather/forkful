---
title:                "Jämförande av två datum"
html_title:           "Fish Shell: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en vanlig uppgift inom programmering, särskilt när man arbetar med tidshantering eller sorteringsfunktioner. Det finns flera situationer där man behöver veta om ett datum är före, efter, eller samma som ett annat datum.

## Så här gör du

Fish Shell erbjuder flera inbyggda funktioner för att enkelt jämföra två datum. Här är några exempel på hur du kan använda dessa funktioner:

```Fish Shell
# Kontrollera om ett datum är efter ett annat datum
if date "2021-01-01" > "2020-12-31"
  echo "2021-01-01 är efter 2020-12-31"
end

# Kontrollera om två datum är samma
if date "2020-10-10" = "2020-10-10"
  echo "2020-10-10 är samma som 2020-10-10"
end

# Kontrollera om ett datum är före ett annat datum
if date "2020-02-14" < "2020-12-24"
  echo "2020-02-14 är före 2020-12-24"
end

# Omvandla datum till sekunder för att jämföra dem
set january_first (date --date "2021-01-01" +%s)
set december_thirtyfirst (date --date "2020-12-31" +%s)
if test $january_first -gt $december_thirtyfirst
  echo "2021-01-01 är efter 2020-12-31"
end

```

I detta exempel utnyttjar vi funktionen `date` för att konvertera specifika datum till sekunder och sedan jämföra dem med hjälp av `test`-kommandot.

## Djupt dyk

När man jämför två datum är det viktigt att tänka på hur tidszoner kan påverka resultatet. Om du inte specifikt anger en tidszon, kommer datumet att tolkas utifrån den aktuella systemtidszonen. Detta kan leda till felaktiga resultat om du arbetar med datum från olika tidszoner.

En annan sak att tänka på är att formatet på dina datum måste vara korrekt för att jämförelsen ska fungera. Fish Shell accepterar många olika format, men det är viktigt att du håller dig till ett konsekvent format för att undvika problem.

## Se även

-https://fishshell.com/docs/current/cmds/date.html
-https://www.geeksforgeeks.org/compare-two-dates-with-the-help-of-fish-shell-in-linux/
-https://stackoverflow.com/questions/52366389/how-to-compare-two-dates-in-bash