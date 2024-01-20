---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:36:02.329170-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en sträng innebär att omvandla textformaterad datuminformation till ett strukturerat format som datorn kan förstå och använda. Programmerare gör detta för att hantera och manipulera datumdata effektivt, som att sortera eller jämföra händelser.

## Så här gör du:
För att parsa ett datum i Fish Shell använder vi `date` kommandot. 

```
set -l date_str "2023-04-01 14:00"
date -d $date_str +"%Y-%m-%d %H:%M"
```
Output:
```
2023-04-01 14:00
```
Ett annat exempel, där vi vill ha veckodagen:

```
set -l date_str "2023-04-01 14:00"
date -d $date_str +"%A"
```
Output:
```
lördag
```

## Fördjupning
Att tolka datum från strängar har länge varit centralt i programmering, eftersom datum och tid är kärnan i så många system. I tidiga datorer var datumhantering oftast begränsad till enkla textsträngar, men med tiden har behovet av mer komplexa beräkningar (som tidszoner och skottår) drivit utvecklingen av mer avancerade funktioner och verktyg, som `date` kommandot i Unix-baserade system.

I Fish Shell är `date` rättfram; vi använder det för att formatera och manipulera datum. Det finns alternativ som Python's `datetime` bibliotek eller JavaScript's Date-objekt som erbjuder liknande funktioner i olika programmeringsmiljöer.

Om vi behöver avancerade tidszonedata eller mer komplex manipulering av datum kan vi använda externa verktyg som `dateutils` eller skripting med ett annat programmeringsspråk.

## Se även
* Fish Shell dokumentation om datumhantering: https://fishshell.com/docs/current/cmds/date.html
* GNU 'date' manual: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
* Dateutils för mer avancerad datum- och tidszonehantering: http://www.fresse.org/dateutils/