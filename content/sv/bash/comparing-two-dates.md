---
title:    "Bash: Jämförande av två datum"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara användbart i många sammanhang, både som en del av automatiska skript och för att få en överblick över tidsramar i ett projekt. Med Bash-programmering kan du enkelt skapa en kod som kan jämföra två datum och ge dig den information du behöver.

## Hur man gör

För att jämföra två datum i Bash behöver du använda kommandot `date` som låter dig hämta dagens datum och tid. Du kan sedan använda olika kommandon och trick för att behandla dessa datum och jämföra dem.

Här är ett enkelt exempel på hur du kan jämföra två datum:

```Bash
#!/bin/bash

today=$(date +"%Y-%m-%d")
birthday="1990-08-20"

if [[ "$today" > "$birthday" ]]; then
  echo "Grattis på födelsedagen!"
else
  echo "Hoppas du får en fin födelsedag!"
fi
```

I det här exemplet börjar vi med att definiera variabeln `today` som innehåller dagens datum i formatet "åååå-mm-dd". Sedan definierar vi variabeln `birthday` med ett födelsedatum som vi vill jämföra med dagens datum. Sedan användar vi `if`-villkoret för att jämföra de två datumen.

Om dagens datum är senare än födelsedatumet, kommer vi att få utskriften "Grattis på födelsedagen!", annars får vi utskriften "Hoppas du får en fin födelsedag!". Detta enkla exempel kan användas som en grund för att bygga mer komplexa jämförelser av datum i dina Bash-skript.

## Djupdykning

Det finns en mängd olika kommandon och trick som kan användas för att hantera och jämföra datum i Bash. Här är några tips och exempel för att hjälpa dig på vägen:

- Använd `date` kommandot för att hämta och formatera datum och tid enligt dina behov.
- Använd `date -d` för att konvertera datum från ett format till ett annat, till exempel från "åååå-mm-dd" till "dd/mm/åååå".
- Du kan använda `date +%s` för att få ut datum i Unix-timestamp format (sekunder sedan 1979-01-01 00:00:00) och sedan jämföra dessa för att se vilket datum som kommer först.
- Om du behöver jämföra ett datum med dagens datum, kan du använda `$(date +%s -d "startdate")` istället för att använda en variabel för startdatumet.
- Du kan också använda `date -d "start date" --date "+$(expr enddate - startdate) days"` för att få datumet som ligger x antal dagar efter startdatumet.

Genom att experimentera med olika kommandon kan du skapa en mer robust kod för att jämföra datum i Bash.

## Se även

Här är några användbara resurser för att lära dig mer om att jämföra datum i Bash:

- [GNU Bash manual](https://www.gnu.org/software/bash/manual/bash.html) - en omfattande resurs för allt som har med Bash-programmering att göra, inklusive hur du hanterar datum.
- [Stack Overflow](https://stackoverflow.com/questions/17309980/bash-compare-dates) - en samling av frågor och svar om att jämföra datum i Bash.
- [GNU Date manual](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html) - en detaljerad guide om hur du använder `date` kommandot för att hantera datum.
- [Bash-hjälp från GNU](https://www.gnu.org/software/bash/) - en resurs som innehåller allt du behöver veta för att bli en mästare på Bash-skript.

Jämföra datum i Bash kan verka som en liten och enkel del av programmering, men det kan spara dig mycket tid och hålla din kod mer organiserad och effektiv