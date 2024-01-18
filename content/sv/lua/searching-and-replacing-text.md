---
title:                "Söka och ersätta text"
html_title:           "Lua: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Textsökning och ersättning är en vanlig uppgift för programmerare. Det handlar om att söka efter specifika ord eller uttryck i en text och ersätta dem med andra ord eller uttryck. Detta kan göras för att korrigera felaktiga stavningar, byta ut föråldrade termer eller för att automatiskt konvertera format.

Programmerare använder sök- och ersättningsfunktioner för att spara tid och minska risken för mänskliga fel. Istället för att manuellt leta och ersätta varje instans av ett ord eller uttryck i en text, kan man skriva ett enkelt program som tar hand om det åt en.

## Hur du gör:
Här är några enkla exempel på hur man kan använda Lua-funktioner för sökning och ersättning:

```Lua
--Sök efter ett ord och ersätt med ett annat
local text = "Jag gillar verkligen ananasjuice."
text = string.gsub(text, "ananas", "apelsin")
-- text blir nu "Jag gillar verkligen apelsinjuice."

--Ta bort ett ord från en text
local text = "Jag gillar inte spenat."
text = string.gsub(text, "inte ", "")
--text blir nu "Jag gillar spenat."
```
Man kan också använda reguljära uttryck för mer avancerad sökning. Till exempel:

```Lua
--Hitta alla siffror i en text och ersätt dem med en asterisk
local text = "42 är ett magiskt nummer."
text = string.gsub(text, "%d+", "*")
--text blir nu "* är ett magiskt nummer."
```

## Djupdykning:
Sök- och ersättningsfunktioner har funnits i programmeringsspråk sedan tidigt 1970-tal. De introducerades för att underlätta textbehandling och substituera text i stora filer. Idag används de fortfarande för samma syfte, men också för att hantera datakonvertering och automatisera uppgifter i webbutveckling och datahantering.

Alternativt till Lua, kan andra språk som Python, Perl och sed användas för sökning och ersättning. Dessa språk erbjuder liknande eller mer avancerade funktioner och det är upp till programmeraren att välja det bäst lämpade språket för sin uppgift.

Implementationen av sök- och ersättningsfunktioner skiljer sig åt mellan olika språk, men de flesta stöder användningen av reguljära uttryck för att söka och matcha mönster i en text. Det är också viktigt att vara noga med hur man hanterar specialtecken som åäö i olika språk.

## Se även:
- [Lua dokumentation om strängar](https://www.lua.org/manual/5.4/manual.html#6.4)
- [En guide till reguljära uttryck i Lua](https://wiki.giderosmobile.com/index.php/Regular_Expressions_in_Lua)