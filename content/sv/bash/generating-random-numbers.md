---
title:                "Bash: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Att generera slumpmässiga nummer är en användbar och rolig funktion i Bash-programmering. Det låter dig skapa en slumpmässig element för spel, skapa testdata, eller helt enkelt för att lägga till variation i dina program. Det är också en bra övning för att förbättra dina programmeringsfärdigheter.

## Hur man gör
Först måste du använda kommandot ```shuf``` för att generera slumpmässiga nummer. Detta kommando blandar inmatningsraderna och skriver ut en slumpmässig ordning av dem. Om du använder flaggan ```-i``` kan du ange ett intervall av nummer som ska genereras. Till exempel:
``` Bash
shuf -i 1-10
```
Detta kommer att generera 10 slumpmässiga nummer mellan 1 och 10.

Om du vill generera slumpmässiga tal med decimaler, kan du använda kommandot ```bc``` tillsammans med ```shuf```. Till exempel:
``` Bash
shuf -i 5-15 | xargs -I {} echo 'scale=2; {}/10' | bc
```
Detta kommer att generera 10 slumpmässiga decimaltal mellan 0,5 och 1,5.

Du kan också använda Bash-variabler för att generera slumpmässiga nummer inuti ett skript. Till exempel:
``` Bash
num=$((RANDOM%100))
echo $num
```
Detta kommer att generera ett slumpmässigt heltal mellan 0 och 99.

## Djupdykning
Det finns olika metoder för att generera slumpmässiga nummer i Bash, men de flesta av dem använder sig av ett pseudoslumpmässigt nummergenerator. Det betyder att resultaten inte är helt slumpmässiga, utan följer en algoritm. För att få mer exakta slumpmässiga nummer, kan du använda tjänster som RANDOM.org som använder verkligt slumpmässiga nummergeneratorer.

Det är också viktigt att komma ihåg att kommandot ```RANDOM``` använder sig av systemets klocka för att generera slumpmässiga tal. Om det inte finns någon klocka eller om klockan är justerad, kan det resultera i icke-slumpmässiga nummer.

Dessutom kan du kombinera flera kommandon för att generera mer komplexa slumpnummer, som till exempel att generera slumpmässiga bokstäver eller ord genom att använda ```shuf``` med alfabetet som inmatning.

## Se även
- [En guide till grundläggande Bash-programmering](https://www.makeuseof.com/tag/bash-script-beginners-guide/)
- [Bash-dokumentationen](https://www.gnu.org/software/bash/)
- [Hämta slumpmässiga nummer från RANDOM.org](https://www.random.org/)