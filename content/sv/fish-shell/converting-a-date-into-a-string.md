---
title:    "Fish Shell: Att konvertera ett datum till en sträng"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng är en vanlig uppgift inom programmering. Det kan vara användbart för att visa datumet på ett mer läsbart sätt eller för att matcha ett datum till ett specifikt format. I denna bloggpost kommer jag att visa hur du kan använda Fish Shell för att enkelt konvertera datum till strängar.

## Så här gör du

För att konvertera ett datum till en sträng i Fish Shell kan du använda dig av kommandot `date` följt av `%F` som specifierar formatet för året, månaden och dagen. Exempelvis:

```Fish Shell
set datum (date +%F)
echo $datum
```

Detta kommer att ge dig ett resultat i formatet `åååå-mm-dd`, exempelvis `2021-05-18`.

Om du vill ange ett annat format kan du använda dig av olika förkortningar för månader och dagar, som `%b` för månadens första tre bokstäver och `%a` för veckodagens första tre bokstäver. Här är ett exempel:

```Fish Shell
set datum (date +%d %b %Y)
echo $datum
```

Detta kommer att ge dig ett resultat som t.ex. `18 May 2021`.

## Djupdykning

Om du vill lära dig mer om hur datumkonvertering fungerar kan du använda dig av kommandot `man date` för att se man-sidan för detta kommando. Där kan du hitta en lista över alla tillgängliga format för att anpassa dina datumsträngar enligt dina behov.

En annan användbar funktion är att lägga till en tidszon till dina datumsträngar. Detta kan göras genom att lägga till `%z` i kommandot, som ger dig resultatet i formatet `+HHMM`, t.ex. `+0200` för svensk tid.

## Se även

- [Fish Shell dokumentation](https://fishshell.com/docs/current/index.html)
- [Man-sidan för kommandot date](https://linuxcommand.org/lc3_man_pages/date1.html)
- [En guide för att formatera datumsträngar med bash](https://www.cyberciti.biz/tips/linux-unix-formatting-dates-for-display.html)