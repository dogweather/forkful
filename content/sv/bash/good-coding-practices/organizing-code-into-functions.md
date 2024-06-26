---
date: 2024-01-26 01:09:13.252003-07:00
description: "Hur man g\xF6r: Skapa en enkel funktion i Bash."
lastmod: '2024-03-13T22:44:38.087879-06:00'
model: gpt-4-1106-preview
summary: Skapa en enkel funktion i Bash.
title: Att organisera kod i funktioner
weight: 18
---

## Hur man gör:
Skapa en enkel funktion i Bash:

```Bash
halsa() {
  echo "Hej, $1!"
}
```

Använd den genom att anropa funktionen med en parameter:

```Bash
halsa "världen"  # Utdata: Hej, världen!
```

Funktioner kan returnera värden med hjälp av `return` för numeriska statuskoder (ej för faktiskt datatillbakagivning):

```Bash
addera() {
  return $(($1 + $2))
}

addera 3 4
echo $?  # Utdata: 7
```

Observera att `$?` fångar upp föregående kommandos returvärde, vilket är det numeriska resultatet av `addera`.

## Fördjupning
I Bash har funktioner varit ett sätt att kompartimentalisera kod sedan de tidiga versionerna. Historiskt sett ligger användningen av funktioner i linje med principerna för strukturerad programmering som introducerades på 1960-talet för att förbättra kodkvaliteten.

Alternativ till funktioner inkluderar att källkoda skriptfiler eller att använda alias, men dessa erbjuder inte samma nivå av modularitet och återanvändning.

En anmärkningsvärd genomförandedetalj i Bash är att funktioner är first-class citizens; de har inget specifikt deklarationsnyckelord som `function` i andra språk, även om `function` är valbart i Bash för läsbarhetens skull. Funktionens omfattning är också intressant - variabler är globala som standard om de inte deklareras som lokala, vilket kan leda till oväntat beteende om det inte hanteras korrekt.

## Se även
- Bash-manual om Shellfunktioner: https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html
- Avancerad Bash-skriptguide: https://tldp.org/LDP/abs/html/functions.html
- "Pro Bash Programming: Scripting the GNU/Linux Shell" för djupgående koncept och praxis för funktionsskriptning.
