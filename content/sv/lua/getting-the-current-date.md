---
title:                "Att få den aktuella datumet"
html_title:           "Lua: Att få den aktuella datumet"
simple_title:         "Att få den aktuella datumet"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få den aktuella datumen är en vanlig uppgift för programmerare. Det innebär att ta reda på den nuvarande kalendern, inklusive årtal, månad och dag.

Att få den aktuella datumen är användbart för många applikationer, särskilt de som är tidskänsliga. Det kan hjälpa till att spåra transaktioner, schemalägga uppgifter och visa användbara anmärkningar för användare.

## Så här gör du:
För att få den aktuella datumen i Lua, kan vi använda funktionen os.date (). Det här är en fördefinierad funktion som returnerar en sträng med den aktuella datumet i ett specifikt format.

```Lua
local datum = os.date("%d/%m/%Y") 
print(datum) 
```

Detta kommer att skriva ut den aktuella ettumsalet i följande format: DD/MM/ÅÅÅÅ.

Om vi vill ha en mer detaljerad aktuella datum, som inkluderar veckodag och timmar, kan vi använda följande kod:

```Lua
local datum = os.date("%A %d %B %Y, %H:%M:%S") 
print(datum) 
```

Detta kommer att skriva ut den aktuella veckodagen, ettumsalet, månaden, året och tiden i timmar, minuter och sekunder.

Förutom att få den aktuella datumen, kan vi också använda os.date () funktionen för att få datum för en viss tidpunkt. Till exempel, om vi vill veta vilket datum det kommer att vara om 100 dagar från nu, skulle vi bara lägga till 100 som ett andra argument till funktionen, som så:

```Lua
local datum = os.date("%d/%m/%Y", os.time() + (100 * 24 * 60 * 60)) 
print(datum) 
```

Detta kommer att ge oss den aktuella datumet om 100 dagar från nu.

## Djupdykning:
Det bästa med att använda os.date () funktionen är att den är lätt att använda och ger oss stort kontroll över den aktuella datumen. Det finns emellertid också andra alternativ för att få den aktuella datumen i Lua, såsom att använda tredjeparts bibliotek eller anropa operativsystemskommandon.

Från ett historiskt perspektiv, var programmerare tidigare tvungna att utveckla sina egna metoder för att få den aktuella datumen. Men med utvecklingen av moderna programmeringsspråk och deras fördefinierade funktioner, har denna uppgift blivit mycket enklare och mer tillgänglig.

För implementeringsdetaljer, kan du hänvisa till Lua-dokumentationen eller community-forum för hjälp och råd.

## Se även:
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/)
- [Lua.org Community Forum](https://luaforum.se/)
- [Alternative Date Libraries for Lua](https://wiki.portal.chalmers.se/cse/pmwiki.php/Main/LuaDateLibraries)