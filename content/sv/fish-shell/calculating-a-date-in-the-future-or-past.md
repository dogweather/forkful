---
title:                "Beräkning av ett datum i framtiden eller förflutna"
html_title:           "Fish Shell: Beräkning av ett datum i framtiden eller förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller förflutna"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Beräkning av datum i framtiden eller förflutna är en vanlig uppgift inom programmering. Det innebär att man tar ett befintligt datum och lägger till eller subtraherar en viss mängd tid för att få ett nytt datum. Detta kan vara användbart för att visa framtida händelser eller för att hantera datum inom ett program.

## Så här gör du:

```Fish Shell
# Lägg till 25 dagar till det befintliga datumet
date -d "+25 days"

# Subtrahera 2 veckor från det befintliga datumet
date -d "-2 weeks"
```

Output:
```
# Lägger till 25 dagar från dagens datum
Thu Apr 4 12:00:00 CEST 2024

# Subtraherar 2 veckor från dagens datum
Thu Mar 21 12:00:00 CET 2024
```

## Djupdykning:

Att räkna ut datum i framtiden eller förflutna är en viktig del av programmering och används ofta i samband med schemaläggning, påminnelser eller hantering av tidsbaserade data.

Det finns flera andra språk och verktyg som också kan användas för denna uppgift, såsom Python, Ruby eller Bash. Det är viktigt att välja det som passar bäst för ditt specifika projekt.

Fish Shell använder sig av GNU date-kommandot för att hantera datumberäkningar. Det är värt att notera att detta kommando kan ha olika syntax beroende på operativsystemet, så se till att kolla den lokala dokumentationen för att säkerställa korrekt implementation.

## Se även:

- Fish Shell dokumentation: https://fishshell.com/docs/current/index.html
- GNU date manual: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Alternativa verktyg för att hantera datum: https://www.redhat.com/sysadmin/date-time-calculations