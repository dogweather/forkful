---
title:    "Bash: Utskrift av felsökningsutdata"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

När man utvecklar programvara, stöter man ofta på problem som gör det svårt att förstå vad som händer i koden. I dessa situationer kan det vara till stor hjälp att lägga till debugutskrifter i ens skript för att få en bättre förståelse för hur programmet beter sig. Debugutskrifter kan också hjälpa till att identifiera eventuella fel och problem i koden.

## Hur man gör

För att skriva debugutskrifter i Bash, kan man använda kommandot "echo". Detta kommando skriver ut en angiven sträng till terminalen. Här är ett enkelt exempel:

```Bash
# Skriver ut "Hello World"
echo "Hello World"
```

Man kan också använda variabler i utskrifterna för att visa värden som ändras under körningen. Till exempel:

```Bash
# Deklarerar en variabel och tilldelar den ett värde
name="John"
# Skriver ut "Hej John"
echo "Hej $name"
```

Det är också möjligt att skriva ut hela variabler eller resultatet av kommandon. Detta görs genom att lägga till ett dollar-tecken ($). Till exempel:

```Bash
# Skriver ut aktuell mapp där skriptet körs
echo "Nuvarande mapp: $PWD"
```

Slutligen, om man vill att utskriften ska visas i en särskild färg för att enklare kunna särskilja den från andra utskrifter, kan man använda "printf" kommandot. Detta kommando ger mer kontroll över utskriften genom att man kan formatera den efter behov. Här är ett exempel:

```Bash
# Skriver ut "Hello" i röd text
printf "\033[31m Hello \033[0m \n"
```

## Djupdykning

Att använda debugutskrifter i Bash kan vara mycket användbart för att få en bättre förståelse för hur ens program beter sig. Det kan också hjälpa till att identifiera eventuella fel och problem i koden. Men det är viktigt att använda debugutskrifter på ett effektivt sätt för att undvika att överflödiga utskrifter förvirrar och överväldigar användaren.

Ett tips är att använda villkorliga utskrifter med hjälp av "if" eller "case" kommandot för att bara visa utskrifter när ett visst villkor är uppfyllt. Detta minskar mängden utskrifter som visas och gör det enklare att hitta relevant information.

Man kan också kombinera debugutskrifter med andra kommandon som "trap" för att få ännu mer information om ett visst programutförande. "Trap" kommandot används för att fånga och hantera signaler som skickas till skriptet. Genom att använda det tillsammans med en debugutskrift, kan man få en överblick av precis när och på vilket sätt signalen uppstår.

## Se även 

- [Bash Guide for Beginners](http://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)
- [Man Pages for echo](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Printing-a-Prompt)
- [Bash Hackers Wiki on Debugging](http://wiki.bash-hackers.org/scripting/debuggingtips)