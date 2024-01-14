---
title:                "Bash: Att få den nuvarande datumet"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Varför
Det finns många anledningar till varför du skulle vilja få dagens datum i Bash-programmering. Det kan vara användbart för att spåra när ett program kördes eller för att skapa dynamiska filnamn baserade på datumet. Oavsett motivet, så är det ganska enkelt att få dagens datum i Bash.

# Hur man gör
Det enklaste sättet att få dagens datum i Bash är genom att använda kommandot `date`. Det ger dig hela datumet, inklusive dag, månad och år, i det format som är inställt i ditt system. Om du vill ha ett specifikt format, kan du använda flaggor som `%m` för månad i numerisk form eller `%A` för veckodag i textform. Här är några exempel på `date` kommandot med olika flaggor:

```Bash
$ date
Tis 30 Mar 2021

$ date +"%d/%m/%y"
30/03/21

$ date +"Idag är det %A den %d:e %B"
Idag är det tisdag den 30:e mars
```

Som du kan se i det sista exemplet kan du även lägga till egen text i kommandot för att få ett mer anpassat resultat. Du kan även använda variabler, som till exempel `$(date +%H)` för att få timmen just nu.

# Djupdykning
`date` kommandot använder sig av systemets inställda tidszon och datumformat. Om du vill ändra något av dessa kan du göra det med flaggorna `-s` och `-R`. Flaggan `-s` låter dig ställa in tiden manuellt genom att ange år, månad, dag, timme, minut och sekund. Flaggan `-R` låter dig byta till ett annat format för att visa datumet. Du kan också använda kommandot `tzselect` för att välja önskad tidszon.

Om du istället vill ha datumet i ett visst antal dagar framåt eller bakåt så kan du göra det med kommandot `date -d` och ett nummer efter det som representerar antalet dagar från idag. Här är ett exempel på hur du skulle få datumet 5 dagar framåt i tiden:

```Bash
$ date -d "+5 days"
Sön 04 Apr 2021
```

# Se även
- `man date` - för mer detaljerad information om `date` kommandot
- [BashGuide - Datum och Tid](https://mywiki.wooledge.org/BashGuide/DatumOchTid) - en guide för att hantera datum och tid i Bash
- [Bash Bash - Datum och Tid](https://tldp.org/HOWTO/Bash-Bash-prompt-HOWTO/x237.html) - en artikel om hur man ändrar datum och tid i Bash-prompten.