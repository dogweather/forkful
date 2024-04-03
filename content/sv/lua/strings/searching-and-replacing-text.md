---
date: 2024-01-20 17:58:50.552621-07:00
description: "How to: (S\xE5 H\xE4r G\xF6r Du:) I Lua \xE4r `string.gsub` din kompis\
  \ f\xF6r att byta ut text. Exempel."
lastmod: '2024-03-13T22:44:38.023612-06:00'
model: gpt-4-1106-preview
summary: "I Lua \xE4r `string.gsub` din kompis f\xF6r att byta ut text."
title: "S\xF6kning och ers\xE4ttning av text"
weight: 10
---

## How to: (Så Här Gör Du:)
I Lua är `string.gsub` din kompis för att byta ut text. Exempel:

```Lua
local originalText = "Hej världen! Världen är stor."
local searchText = "världen"
local replaceText = "universum"
local resultText = originalText:gsub(searchText, replaceText)
print(resultText)  -- Output: Hej universum! Universum är stor.
```

För att hantera stora/små bokstäver:

```Lua
local caseInsensitiveResult = originalText:gsub(searchText:lower(), replaceText:lower())
print(caseInsensitiveResult)  -- Output: Hej universum! Universum är stor.
```

Och med mönster:

```Lua
local patternResult = originalText:gsub("(%l+)världen", "%1universum")
print(patternResult)  -- Output: Hej universum! universum är stor.
```

## Deep Dive (Djupdykning):
`string.gsub` introducerades i Lua för att förenkla textbearbetning. Det är inte lika kraftfullt som regexp i andra språk, men det funkar bra för enkla ändringar och mönstermatchning. Alternativen, som lpeg-biblioteket, erbjuder kraftfullare mönstermatchning men kan vara överkurs för enkla ändamål.

I Lua opererar `gsub` på strings, där det första argumentet är den sträng man vill bearbeta, det andra är mönstret att söka efter, och det tredje är strängen eller en funktion för ersättningen. Den kan även returnera antalet ersättningar som gjorts, vilket kan vara nyttigt för att få feedback på operationen.

```Lua
local result, numReplacements = originalText:gsub(searchText, replaceText)
print(result, numReplacements)  -- Output: Hej universum! Universum är stor. 2
```

OBS! Mönstren i `gsub` är Lua-specifika och följer inte samma regler som vanliga uttryck, så läs upp på dem innan användning.

## See Also (Se Även):
- Lua's string patterns documentation: https://www.lua.org/pil/20.2.html
- Lua's reference manual `string.gsub`: https://www.lua.org/manual/5.4/manual.html#pdf-string.gsub
- An introduction to Lua's pattern matching: https://lua-users.org/wiki/PatternsTutorial

Dessa länkar leder till engelska sidor, då det kan vara brist på material på svenska för dessa ämnen. Men de är bra resurser för att fördjupa förståelsen inom området.
