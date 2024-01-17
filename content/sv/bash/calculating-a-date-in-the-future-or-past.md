---
title:                "Beräkna ett datum i framtiden eller det förflutna"
html_title:           "Bash: Beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att beräkna ett datum i framtiden eller förflutna är en vanlig uppgift för programmerare. Det görs ofta för att automatisera tidsrelaterade funktioner eller planeringsaktiviteter för olika applikationer.

## Hur man gör:
För att beräkna ett datum i framtiden eller förflutna i Bash, används kommandot ```date```. För att lägga till ett visst antal dagar till det aktuella datumet, kan du använda flaggan "-d" och antalet dagar som du vill lägga till. Till exempel: 

```Bash
date -d "+10 days"
```
Detta kommer att ge dig datumet i tiden efter 10 dagar från och med idag. Du kan också använda andra tidsenheter som veckor, månader eller år i samma format. Om du vill beräkna ett datum i det förflutna istället för i framtiden, lägg till ett "-" tecken före antalet. Till exempel: 

```Bash
date -d "-2 months"
```
Detta kommer att ge dig datumet två månader tidigare från och med idag.

## Fördjupning:
Att kunna beräkna datum i framtiden eller förflutna är en viktig del av programmering eftersom det ger möjlighet att automatisera tidsbaserade uppgifter och funktioner. Det är också ett viktigt verktyg för att hantera schemaläggning och datumrelaterade data i olika applikationer.

Det finns också andra sätt att beräkna datum i Bash, som att använda verktyg som "dateutils" eller "dtpick". Dessa verktyg ger mer mångsidighet och exakt datumberäkning.

När det gäller implementationen använder kommandot "date" sig av Unix-tidsformatet, som är antalet sekunder som har gått sedan 1 januari 1970. Detta gör det möjligt att lägga till eller subtrahera en viss mängd sekunder från det aktuella datumet.

## Se även:
Här är några bra resurser för att lära dig mer om hur man beräknar datum i Bash: 

- [Officiell dokumentation för kommandot "date"](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- ["dateutils" hemsida](https://www.fresse.org/dateutils/)
- [Om Unix-tidsformatet](https://www.epochconverter.com/)