---
title:                "Få det aktuella datumet"
aliases:
- /sv/bash/getting-the-current-date.md
date:                  2024-02-03T19:08:58.616225-07:00
model:                 gpt-4-0125-preview
simple_title:         "Få det aktuella datumet"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta det aktuella datumet i Bash innebär att använda inbyggda kommandon för att visa datum och tid i olika format. Programmerare använder denna funktionalitet för uppgifter som att tidsstämpla loggar, schemalägga uppgifter eller bara som en del av sina systeminformationsskript för att spåra när åtgärder genomfördes.

## Hur man gör:
I Bash är `date`-kommandot ditt primära verktyg för att få det aktuella datumet och tiden. Här är några exempel på hur man använder det:

1. **Hämta det aktuella datumet och klockslaget i det förvalda formatet:**

```bash
date
```

*Exempel på utdata:*
```
Wed Apr 5 14:22:04 PDT 2023
```

2. **Anpassa utdataformatet:** Du kan specificera utdataformatet med hjälp av `+%` formatspecifierare. Till exempel, för att visa datumet i YYYY-MM-DD-format:

```bash
date "+%Y-%m-%d"
```

*Exempel på utdata:*
```
2023-04-05
```

3. **Hämta det aktuella UNIX-tidsstämpeln:** UNIX-tidsstämpeln är antalet sekunder sedan Unix-epoken (1 januari 1970). Detta är användbart för skript som utför beräkningar baserade på tidsskillnader.

```bash
date "+%s"
```

*Exempel på utdata:*
```
1672877344
```

Inga populära tredjepartslibrerier används vanligtvis för denna grundläggande operation i Bash eftersom det inbyggda `date`-kommandot tillhandahåller omfattande funktionalitet. Dock, för mer avancerade datum- och tidsmanipulationer, kan programmerare använda andra programmeringsspråk eller verktyg som erbjuder bibliotek för datumaritmetik och parsing, såsom Pythons `datetime`-modul.
