---
title:                "Omvandla ett datum till en sträng"
html_title:           "Fish Shell: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Att konvertera en datum till en sträng är en vanlig uppgift för programmerare. Det gör att datumet kan läsas och hanteras på ett enkelt sätt av programmet eller användaren. Det kan också behövas för att jämföra datum eller för att visa datumet i en önskad format.

# Hur man gör:

```Fish Shell
# Skapa en variabel med det aktuella datumet
set idag (date)

# Visa datumet i standardformatet YYYY-MM-DD
echo $idag

# Visa datumet i ett anpassat format
echo (strftime "%d/%m/%Y" $idag) 
```
Resultat:
```
2020-10-28
28/10/2020
```

# En djupare titt:

- Att konvertera datum till strängar har varit ett problem sedan de första programmeringsspråken skapades. Det finns olika metoder för att hantera det, men i Fish Shell använder vi inbyggda funktioner som `date` och `strftime`.
- Alternativ till Fish Shell för att konvertera datum till strängar är till exempel Bash, Python och Ruby. Varje språk har sina egna inbyggda funktioner eller metoder för att utföra uppgiften.
- Fish Shell använder sig av C-programmeringsspråket för implementationen av sina inbyggda funktioner för datum- och tidsbehandling.

# Se även:

- [Fish-Shell dokumentation om date](https://fishshell.com/docs/current/cmds/date.html)
- [Wikipedia artikel om datum- och tidsrepresentation](https://en.wikipedia.org/wiki/Date_and_time_representation_by_country)
- [Bash's inbyggda funktion `date`](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)
- [Python's modul datetime](https://docs.python.org/3/library/datetime.html)
- [Ruby's inbyggda funktion `time`](https://ruby-doc.org/core-2.5.0/Time.html)