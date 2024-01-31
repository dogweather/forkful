---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"

category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck är mönster för att matcha textsträngar. De används för att söka, redigera och manipulera text snabbt och effektivt.

## Hur gör man:
```Lua
local text = "Hej, det är Lua-programmering!"
-- Hitta alla ord som börjar med 'p'
for word in text:gmatch("%fp%w+") do
    print(word)
end
-- Output: programmering
```
Enkelt mönster för att validera ett svenskt personnummer:
```Lua
local pnr = "850709-1234"
local match = string.match(pnr, "^%d%d%d%d%d%d%-%d%d%d%d$")
if match then
    print("Giltigt personnummer.")
else
    print("Ogiltigt personnummer.")
end
-- Output: Giltigt personnummer.
```

## Fördjupning
Reguljära uttryck är inte nytt; de kom på 1950-talet i automatteori och formaliserades av Stephen Kleene. Lua har inte standard regexp-stöd som Perl eller Java, men erbjuder mönstermatchning som täcker många användningsfall. Lua-mönster är enklare men kan vara mindre kraftfulla. Att använda externa bibliotek som `lrexlib` ger tillgång till fulla reguljära uttryck.

## Se Även
- Lua-users Wiki om mönstermatchning: http://lua-users.org/wiki/PatternsTutorial
- Online Lua mönster matchning verktyg: https://www.lua.org/cgi-bin/demo
- `lrexlib` dokumentation för avancerad mönstermatchning: https://github.com/rrthomas/lrexlib
