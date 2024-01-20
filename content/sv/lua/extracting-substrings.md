---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Extrahering av delsträngar innebär att plocka ut en specifik del av en sträng i programmering. Programmerare gör detta för att återanvända, manipulera eller jämföra dessa delsträngar för diverse ändamål.

## Så här gör du:

Lua erbjuder oss `string.sub()`-funktionen för att extrahera delsträngar. Funktionen tar tre argument: strängen att bearbeta, startpositionen och slutpositionen.

```Lua
str = "Hej, Världen!"
print(string.sub(str, 1, 3))  -- Output: "Hej"
```
I detta exempel indiceras strängen från 1, så "Hej" börjar vid position 1 och slutar vid position 3.

## Djupdykning

Historiskt sätt är extrahering av delsträngar ett grundläggande behov inom textbearbetning, därför har det varit en del av standardsträngbiblioteket sedan Luas tidiga dagar. 

Alternativt kan vi använda `string.find()` och `string.match()` för att hitta och extrahera delsträngar, särskilt när det handlar om komplexa mönster eller reguljära uttryck.

Implementationen av `string.sub()` i Lua använder faktiskt C standarbibliotekets `strncpy`-funktion, vilket gör det mycket effektivt. Kom ihåg att Lua är 1-indexerat, inte 0-indexerat som många andra programmeringsspråk.

## Se också

1. [Officiell Lua Dokumentation](https://www.lua.org/manual/5.4/manual.html): För en fullständig genomgång av alla strängfunktioner i Lua. 
2. [Lua-Users Wiki](http://lua-users.org/wiki/): Ett bra ställe för användarbidrag och avancerade ämnen.