---
title:                "Bash: Att konvertera ett datum till en sträng"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Det är vanligt att man behöver konvertera datum till strängar i Bash programmering. En vanlig anledning kan vara att man vill visa datum på ett specifikt format eller inom en viss tidszon. I denna bloggpost kommer jag visa hur man kan göra detta på ett enkelt sätt.

## Hur man gör det

För att konvertera datum till strängar i Bash, kan man använda kommandot `date` följt av formatsträngen "%Y-%m-%d". Nedan följer ett exempel på hur man kan använda detta kommando:

```bash
date +%Y-%m-%d
```

Detta kommer att ge output som ser ut så här: 2021-03-18. Formatsträngen "%Y-%m-%d" står för år, månad och dag och det är därför dessa värden visas i outputen.

Man kan också lägga till andra värden i formatsträngen för att visa mer specifika datum. Till exempel kan man lägga till "%H:%M:%S" för att även visa timmar, minuter och sekunder.

```bash
date +%Y-%m-%d%H:%M:%S
```

Detta kommer att ge output som ser ut så här: 2021-03-18 15:27:12. Som ni kan se har vi nu fått med tidsinformationen också.

## Djupdykning

Vill man få ut datum och tid inom en specificerad tidszon, kan man använda flaggan "-u" efter `date` kommandot. Till exempel om man vill få ut datumet i New Yorks tidszon kan man använda följande kommando:

```bash
date -u -d "now" +"%Y-%m-%d %H:%M:%S" -D "%Z"
```

Outputen kommer då att se ut så här: 2021-03-18 11:27:12 EDT, där "EDT" är förkortningen för Eastern Daylight Time.

Om man vill se en lista med alla olika formatsträngar som man kan använda kan man skriva `man date` i terminalen för att öppna manualen för kommandot. Där hittar man en utförlig beskrivning av alla möjliga formatsträngar och hur de används.

## Se även

- [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [DATE (Linux manual page)](https://linux.die.net/man/1/date)
- [Bash String Manipulation Basics](https://www.baeldung.com/linux/bash-string-manipulation)