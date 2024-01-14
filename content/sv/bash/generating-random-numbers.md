---
title:                "Bash: Generering av slumpmässiga nummer"
programming_language: "Bash"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför 

Att generera slumpmässiga nummer kan vara väldigt användbart för många olika ändamål. Det kan användas för att testa program, skapa unika lösenord, eller till och med för spel och underhållning.

## Så här gör du 

För att generera slumpmässiga nummer i Bash, kan du använda dig av kommandot "shuf". Detta kommando slumpar om innehållet i en fil eller en lista. Här är ett exempel på hur du kan använda det för att generera slumpmässiga nummer mellan 1 och 10: 

```Bash 
shuf -i 1-10 -n 1 
``` 

Detta kommer att slumpmässigt välja ett nummer mellan 1 och 10 och skriva ut det på skärmen. Du kan också ange antalet nummer du vill generera genom att ändra värdet på "-n" parameter. Till exempel, om vi ändrar värdet till 5 kommer det att generera 5 slumpmässiga nummer istället för bara ett. 

## Djupdykning 

Det finns många olika sätt att generera slumpmässiga nummer i Bash, inklusive användning av andra kommandon som "od" och "awk". Det är också möjligt att generera slumpmässiga strängar istället för bara numeriska värden. Detta kan vara särskilt användbart för att skapa unika lösenord. 

## Se även 

- [Random Number Generation in Bash](https://www.shell-tips.com/2010/06/14/introduction-to-random-numbers/)
- [Generating Passwords in Bash](https://linuxconfig.org/generating-your-own-random-passwords-on-the-linux-command-line)
- [Bash Built-in Commands](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Bash-Builtins)