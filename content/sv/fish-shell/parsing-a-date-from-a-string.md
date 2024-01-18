---
title:                "Att tolka ett datum från en sträng"
html_title:           "Fish Shell: Att tolka ett datum från en sträng"
simple_title:         "Att tolka ett datum från en sträng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ”parsa” ett datum från en sträng är när man tar en textsträng och konverterar den till ett datumformat som datorn kan förstå och hantera. Detta är vanligtvis användbart när man hanterar användarinput i ett program eller när man behandlar data från en annan källa. Det är en nödvändig del av programmering för att kunna hantera datum och tider på ett effektivt sätt.

## Så här gör du:

```Fish Shell 
set date (date -f %m/%d/%Y 11/20/2021)
echo $date
```
Output: 20 Nov 2021

I det här exemplet använder vi Fish Shell för att konvertera en textsträng i formatet månad/dag/år till ett datum med hjälp av kommandot "date" och formatet "%m/%d/%Y". Genom att sedan använda "echo" kan vi skriva ut det konverterade datumet till terminalen.

```Fish Shell
set date (date -f %d%m%Y 20210712)
echo $date
```
Output: 12 Jul 2021

I det här exemplet kan vi se hur formatet på inputsträngen påverkar utgången av det konverterade datumet. Genom att ändra formatet till "%d%m%Y" förändras ordningen på dag och månad i det utskrivna datumet.

## Djupdykning:

Att konvertera datum från strängar är något som har funnits sedan de tidiga dagarna av dataprogrammering. Det är en viktig del av att hantera kalenderfunktioner och tidshantering inom datorer. Alternativ till att använda Fish Shell för detta inkluderar andra kommandoradbaserade skal som Bash eller Zsh, men också mer komplexa programmeringsspråk som Python eller Java.

Något att tänka på när man arbetar med att parsea datum från strängar är att se till att formatet på strängen matchar formatet som används för konverteringen. Om formatet inte matchar kan det leda till felaktiga eller oförutsägbara resultat. Det kan också vara viktigt att se till att det konverterade datumet hanteras på rätt sätt, till exempel att det inte finns några felaktiga datum som den 29 februari när det inte är ett skottår.

## Se även:

- [Fish Shell dokumentation för kommandot "date"](https://fishshell.com/docs/current/cmds/date.html)
- [Unix timestamp konverterare för att parsad datum och tid till olika format](https://www.unixtimestamp.com/)
- [En guide till att hantera datum och tider i programmeringsspråket Python](https://realpython.com/python-datetime/)