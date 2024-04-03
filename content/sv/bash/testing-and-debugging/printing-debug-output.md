---
date: 2024-01-20 17:52:02.749103-07:00
description: "How to: F\xF6r att visa fels\xF6kningsinformation i Bash anv\xE4nder\
  \ du `echo` eller `printf`. H\xE4r \xE4r exempel."
lastmod: '2024-03-13T22:44:38.085012-06:00'
model: gpt-4-1106-preview
summary: "F\xF6r att visa fels\xF6kningsinformation i Bash anv\xE4nder du `echo` eller\
  \ `printf`."
title: "Skriva ut fels\xF6kningsdata"
weight: 33
---

## How to:
För att visa felsökningsinformation i Bash använder du `echo` eller `printf`. Här är exempel:

```Bash
# Använd echo för att skriva ut enkel text
echo "Det här är en debug-meddelande"

# Skriv ut variabelvärden
debug_variable="ett värde"
echo "Debug: variabeln har värdet '$debug_variable'"

# Avancerad: Använd printf för formatering
printf "Filen %s har %d rader\n" $filnamn $radantal
```

Exempel på output:

```
Det här är en debug-meddelande
Debug: variabeln har värdet 'ett värde'
Filen script.sh har 42 rader
```

## Deep Dive
Debugging uppstod ur behovet att förstå vad som hände inuti datorer. På 1940-talet tog operatörer bort faktiska insekter ur hålkortsläsare för att "debugga" problem, därav termen. 

I Bash-scripting kommer `echo` och `printf` ofta till användning för att skriva ut felsökningsmeddelanden. `echo` är enklare men mindre flexibel. `printf` erbjuder formatering, vilket är användbart om du behöver mer kontroll över hur output ser ut. 

Bash-version 4 introducerade `printf -v` som tillåter dig att tilldela formatsträngars resultat direkt till variabler, utan subshell.

För att undvika att felsökningsmeddelanden blandas med vanlig output kan du använda skilda filströmmar. Standard är filström 1 för normal output och filström 2 för error-meddelanden. Lägg till `1>&2` för att skicka en debug-meddelande till felströmmen.

Exempel:

```Bash
echo "Detta kommer att synas i terminalen" 1>&2
```

Du kan även omdirigera dessa meddelanden till en fil eller en annan kommand för vidare analys.

## Se Även
- [Bash man-page](https://linux.die.net/man/1/bash) för en djupdykning i Bash.
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/) för mer avancerade tekniker och exempel.
- [ShellCheck](https://www.shellcheck.net/) för att hitta och förbättra skript.
