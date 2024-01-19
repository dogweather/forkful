---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Fish Shell: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att beräkna ett datum i framtiden eller förflutna handlar om att skapa ett datum baserat på en viss tidsförskjutning från nuvarande datum. Det är vanligt bland programmerare för allt från schemaläggning av uppgifter till att skapa tidslinjer för projektplanering.

## Hur man gör:

I Fish Shell kan vi använda 'date' kommandot för att beräkna datum. Enkel syntax kan se ut så här:

```fish
date -u -d "+30 days"
```

Detta kommer att returnera ett datum 30 dagar framåt. 

Om vi vill gå tillbaka i tiden, kan vi ändra tecknet till minus:

```fish
date -u -d "-30 days"
```

Detta ger ett datum 30 dagar i det förflutna.

## Djup dykning:

Historiskt sett har beräkning av datum baserat på en tidsförskjutning varit en grundläggande uppgift inom programmering, och det är därför det har en stark representation i UNIX-tidskommandot 'date'.

När det gäller alternativ till datumkommandot i Fish Shell, kan du använda mer kraftfulla verktyg som 'DateTime' biblioteket i språk som Python eller JavaScript. Dessa verktyg erbjuder mer sofistikerade metoder för datumhantering.

När det gäller att implementera detta i Fish Shell, beror det på 'date' kommandot som anroper systemkärnan för att få det aktuella datumet och tiden, och sedan tillämpar det mängden tid ni önskar för att ge det begärda datumet.

## Se också:

För mer information om 'date' kommandot och dess användning, kolla in dess man-sida:  [http://man7.org/linux/man-pages/man1/date.1.html](http://man7.org/linux/man-pages/man1/date.1.html)

För mer avancerad datumhantering i programmeringsspråk som Python, se 'DateTime' biblioteket: [https://docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)