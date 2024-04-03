---
date: 2024-01-20 17:35:11.043801-07:00
description: "Hur man g\xF6r: F\xF6r att sammanfoga str\xE4ngar i Lua anv\xE4nder\
  \ du `..` operatorn. S\xE5 h\xE4r."
lastmod: '2024-03-13T22:44:38.030204-06:00'
model: gpt-4-1106-preview
summary: "F\xF6r att sammanfoga str\xE4ngar i Lua anv\xE4nder du `..` operatorn."
title: "Sammanslagning av str\xE4ngar"
weight: 3
---

## Hur man gör:
För att sammanfoga strängar i Lua använder du `..` operatorn. Så här:

```Lua
local hej = "Hej"
local varld = "världen"
local meddelande = hej .. ", " .. varld .. "!"
print(meddelande)  -- Output: Hej, världen!
```

Du kan även använda `table.concat` för arrayer av strängar:

```Lua
local ord = {"Hur", "mår", "du", "?"}
local mening = table.concat(ord, " ")
print(mening)  -- Output: Hur mår du ?
```

## Djupdykning
Sammanfogning av strängar är en grundläggande operation som funnits i många programmeringsspråk genom åren. I Lua är `..` operatorn inbyggd och effektiv för korta och enkla sammanfogningar. För större mängder text kan prestandan dock vara viktig; att använda `table.concat` är oftast snabbare, särskilt när det handlar om att bygga upp stora strängar eftersom det minskar antalet temporära strängobjekt som skapas.

Strängar i Lua är immutable, vilket innebär att en ny sträng skapas varje gång du utför en sammanfogning. Denna design kan påverka prestandan i program med omfattande strängbearbetning, vilket gör att `table.concat` tillhandahåller ett värdefullt alternativ.

## Se även
- [Lua 5.4 Reference Manual: Strings](https://www.lua.org/manual/5.4/manual.html#6.4)
- [Programming in Lua: Manipulating Strings](https://www.lua.org/pil/11.6.html)
