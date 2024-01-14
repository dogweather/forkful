---
title:    "Bash: Att få den nuvarande datumet"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att kunna hämta och använda dagens datum är en användbar funktion i många olika sammanhang, oavsett om du använder Bash för personligt bruk eller i ett professionellt sammanhang. Det kan hjälpa dig hålla koll på deadlines, organisera dina filer eller visa relevanta datum för olika uppgifter.

## Hur man gör
Det finns flera olika sätt att hämta dagens datum i Bash. Ett vanligt sätt är att använda kommandot "date" följt av lämpliga flaggor för att få önskad formatering. Här är ett exempel på hur du kan få dagens datum i svensk format:

```Bash
date +"%d/%m/%Y"
```

Detta skulle ge utskriften "14/05/2021". Om du vill ha veckodagen med i utskriften kan du lägga till flaggan "+%A" för att få namnet på veckodagen (till exempel "fredag"). Här är ett annat exempel där vi kombinerar både datum och veckodag:

```Bash
date +"%A, %d %B, %Y"
```

Som ger utskriften "fredag, 14 maj, 2021". Det finns också andra flaggor som du kan använda för att få ut mer exakt information, till exempel "+%Y" för att få utskriften av det aktuella året eller "+%j" för att få numret på dagen i det aktuella året. Du hittar en lista över alla tillgängliga flaggor genom att söka på "Bash date flags" på nätet.

## Djupdykning
Om du vill ha mer information och kontroll över hur du hämtar dagens datum kan du använda kommandot "cal" istället för "date". "cal" ger dig en kalender för det aktuella året och du kan sedan använda lämpliga flaggor för att få ut information om specifika datum. Här är ett exempel där vi använder flaggan "-y" för att få ut hela året:

```Bash
cal -y
```

Detta skulle ge utskriften av hela året med markeringar för aktuell månad och dag. Du kan också använda flaggan "-d" för att få information om ett specifikt datum, till exempel:

```Bash
cal -d "14 may"
```

Som skulle ge utskriften "14 maj" med markering för aktuell dag.

## Se även
- [Bash Reference Manual - Date and Time](https://www.gnu.org/software/bash/manual/html_node/Date-and-Time.html)
- [Linuxize - How to Get Current Date and Time in Bash Script](https://linuxize.com/post/bash-get-current-date-time/)
- [DigitalOcean - How To Use the `date` Command in Linux](https://www.digitalocean.com/community/tutorials/how-to-use-the-date-command-in-linux)