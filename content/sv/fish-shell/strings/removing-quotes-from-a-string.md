---
date: 2024-01-26 03:38:53.400816-07:00
description: "Att ta bort citattecken fr\xE5n en str\xE4ng handlar om att rensa bort\
  \ de d\xE4r irriterande enkla (' ') eller dubbla (\" \") citationstecknen fr\xE5\
  n din textdata.\u2026"
lastmod: '2024-03-13T22:44:38.323597-06:00'
model: gpt-4-0125-preview
summary: "Att ta bort citattecken fr\xE5n en str\xE4ng handlar om att rensa bort de\
  \ d\xE4r irriterande enkla (' ') eller dubbla (\" \") citationstecknen fr\xE5n din\
  \ textdata."
title: "Ta bort citattecken fr\xE5n en str\xE4ng"
weight: 9
---

## Hur gör man:
Fish har inbyggd magi för denna typ av uppgift. Använd `string`-funktionen utan att bryta en svett. Kika på dessa trollformler:

```fish
# Exempel med enkla citattecken
set quoted "'Hej, världen!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # Utdata: Hej, världen!

# Samma sak med dubbla citattecken
set double_quoted "\"Hej, universum!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # Utdata: Hej, universum!
```

## Djupdykning
Tillbaka i kommandoradens stenålder, skulle du brottas med `sed` eller `awk` för att ta bort citattecken; en riktig djungel av omvända snedstreck och kryptiska flaggor. Fishs `string`-funktion är från en nyare era, vilket gör koden renare och mer intuitiv.

Alternativ i andra skal kan fortfarande luta sig på dessa gamla verktyg eller kan använda sina egna inbyggda metoder som bashs parameterexpansion eller zshs modifierare.

`string`-funktionen går utöver att trimma citattecken. Det är en schweizisk armékniv för strängoperationer i Fish. Med `string` kan du skiva, tärna, dela, sammanfoga, eller till och med regex-matcha strängar direkt i din terminal.

## Se också
Fördjupa dig i `string` med hjälp av den officiella dokumentationen:
- [Fish Shell Strängdokumentation](https://fishshell.com/docs/current/commands.html#string)

För nostalgi eller när du skriptar med mer traditionella skal, kolla in:
- [Sed & Awk-guide](https://www.grymoire.com/Unix/Sed.html)
- [Bash Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
