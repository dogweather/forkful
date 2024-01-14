---
title:    "Bash: Omvandla ett datum till en sträng"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Varför

 Att konvertera ett datum till en sträng kan vara användbart i många olika program eller skript, särskilt när man behöver visa eller spara datumet på ett mer läsbart sätt.

##Hur man gör det

För att konvertera ett datum till en sträng i Bash kan man använda kommandot `date` följt av önskat datumformat och önskat datum. Till exempel, om vi vill konvertera dagens datum till formatet "DD/MM/YYYY", skulle kommandot se ut enligt följande:

```Bash
date +"%d/%m/%Y"
```

Detta skulle ge oss ett resultat som ser ut som "06/02/2021".

Om vi vill konvertera ett specifikt datum istället för dagens datum, kan vi ange det datum som ett argument efter `date`-kommandot. Till exempel:

```Bash
date +"%d/%m/%Y" 20/12/2020
```

Detta skulle ge oss resultatet "20/12/2020".

##Djupdykning

Det finns många olika format i vilka man kan konvertera datum till strängar med hjälp av `date`-kommandot. Här är några exempel på vanliga format och vad de betyder:

- `%d` - Dag i månaden (t.ex. 06)
- `%m` - Månad (t.ex. 02)
- `%Y` - År (t.ex. 2021)
- `%H` - Timme i 24-timmarsformat (t.ex. 15)
- `%M` - Minut (t.ex. 30)
- `%S` - Sekund (t.ex. 45)

Det finns också många andra formatalternativ som du kan använda för att få mer specifika resultat. För en komplett lista över alla alternativ kan du titta i `date`-kommandots man-sida genom att köra kommandot `man date` i terminalen. Där kan du också se olika exempel på hur man kan använda `date` för att konvertera datum till olika format.

## Se även

- [GNU Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)
- [How to Use the date Command in Linux](https://linuxize.com/post/how-to-use-date-command-in-linux/)