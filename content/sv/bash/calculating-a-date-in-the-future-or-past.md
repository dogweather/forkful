---
title:    "Bash: Beräkning av ett datum i framtiden eller det förflutna"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##Varför

Att kunna räkna ut ett datum i framtiden eller i förflutnan kan vara användbart i många olika situationer. Det kan hjälpa dig att planera evenemang, boka resor eller helt enkelt hålla koll på viktiga datum.

##Så här gör du

Att beräkna ett datum i framtiden eller förflutna kan enkelt göras med hjälp av Bash-programmering. Först behöver du ange ett datum i form av år, månad och dag. Sedan kan du använda ett enkelt Bash-kommando för att lägga till eller dra bort ett antal dagar från datumet.

Här är ett exempel på hur du kan beräkna ett datum i framtiden:

```Bash
date -d "2021-12-01 + 7 days"
```

Detta kommando kommer att ge dig datumet för en vecka efter det angivna datumet, i det här fallet den 8:e december 2021. Om du vill beräkna ett datum i förflutna behöver du bara byta ut "plus" mot "minus" i kommandot.

##Djupdykning

För att förstå hur detta fungerar behöver vi veta att Bash använder sig av Unix-tiden, som är antalet sekunder som har gått sedan 1 januari 1970. När du lägger till eller drar bort dagar från ett datum, konverteras detta datum till Unix-tiden och sedan omvandlas tillbaka till datumformatet.

För att ta reda på Unix-tiden för ett visst datum kan du använda följande kommando:

```Bash
date -d "2021-12-01" +%s
```

Detta kommer att returnera antalet sekunder som har gått sedan 1970-01-01 00:00:00 UTC fram till detta datum. Sedan för att omvandla tillbaka till ett datum kan du använda följande kommando:

```Bash
date -d @${unix_time}
```

Här ska du ersätta ${unix_time} med det värde du fick från det förra kommandot.

##Se även

- [Bash man-sidan för date](https://ss64.com/bash/date.html)
- [Unix-tiden på Wikipedia](https://sv.wikipedia.org/wiki/Unix-tid)