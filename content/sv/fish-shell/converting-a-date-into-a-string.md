---
title:                "Fish Shell: Omvandling av datum till sträng"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna konvertera datum till strängar är en användbar funktion när du arbetar med Fish Shell-programmering. Detta kan hjälpa dig att visa datum i ett läsbart format eller använda det för att sortera och filtrera data.

## Så här gör du
För att konvertera ett datum till en sträng i Fish Shell, kan du använda kommandot `date`. Det finns flera olika format som du kan använda för att anpassa utseendet på den genererade strängen. Här är några exempel:

```Fish Shell
# Visa dagens datum i formatet "YYYY-MM-DD"
date +%F

# Visa dag, månad, år och timme i formatet "DD.MM.YYYY HH:MM"
date +"%d.%m.%Y %H:%M"

# Visa datumet med fullständigt namn på månaden och veckodagen
date +"%A, %B %d, %Y"
```

Output:

```
2020-08-07
07.08.2020 15:30
Friday, August 07, 2020
```

Det finns många fler formatalternativ som du kan använda beroende på dina behov. Du kan också kombinera flera format för att skapa dina egna anpassade datumsträngar.

## Djupgående
När du använder `date` för att konvertera datum till strängar, är det viktigt att förstå hur det fungerar. Detta kommando använder sig av din systemklocka för att hämta aktuellt datum och tid. Det betyder att du kan få olika resultat beroende på inställningarna för din systemklocka.

Det är också värt att nämna att kommandot `date` är en del av paketet `coreutils`, vilket betyder att det kan variera beroende på vilken version du använder. Se till att kolla dokumentationen för att hitta de rätta formatalternativen för din version.

## Se även
- [Fish Shell Wiki](https://github.com/fish-shell/fish-shell/wiki)
- [Date(1) manual page](https://fishshell.com/docs/current/cmds/date.html)
- [Coreutils Date documentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)