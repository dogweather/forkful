---
date: 2024-01-26 03:50:18.449356-07:00
description: "En debugger \xE4r ett verktyg som l\xE5ter dig inspektera och kontrollera\
  \ utf\xF6randet av ett program, vilket g\xF6r det enkelt att identifiera var saker\
  \ g\xE5r fel.\u2026"
lastmod: '2024-02-25T18:49:36.356897-07:00'
model: gpt-4-0125-preview
summary: "En debugger \xE4r ett verktyg som l\xE5ter dig inspektera och kontrollera\
  \ utf\xF6randet av ett program, vilket g\xF6r det enkelt att identifiera var saker\
  \ g\xE5r fel.\u2026"
title: "Att anv\xE4nda en debugger"
---

{{< edit_this_page >}}

## Vad & Varför?
En debugger är ett verktyg som låter dig inspektera och kontrollera utförandet av ett program, vilket gör det enkelt att identifiera var saker går fel. Programmerare använder debuggers för att krossa buggar, förstå kodflödet, och för att säkerställa att deras kod är så ren som en visselpipa.

## Hur gör man:
Lua kommer inte med en inbyggd debugger, men du kan använda externa sådana, som ZeroBrane Studio. Här är en smakprov på hur du skulle arbeta med den:

```Lua
-- Detta är ett enkelt Lua-skript med ett medvetet fel
local function add(a, b)
    local result = a + b -- Oj, låt oss låtsas att vi glömde definiera 'b'
    return result
end

print(add(10))
```

När du kör detta i en debugger kommer den att pausa utförandet där saker stökar till sig. Du kommer att se något i stil med detta:

```
lua: example.lua:3: försök att utföra aritmetik på ett nil-värde (lokal 'b')
stack traceback:
	example.lua:3: i funktionen 'add'
	example.lua:7: i huvudblock
	[C]: i ?
```

Du kan sätta brytpunkter, stega igenom din kod, och titta på variabelvärden för att spåra buggen utan att förlora ditt förstånd.

## Djupdykning
Lua's enkelhet sträcker sig tyvärr inte till felsökning. Ingen fara dock, Lua-gemenskapen har din rygg. Verktyg som ZeroBrane Studio, LuaDec och andra erbjuder felsökningsmöjligheter. Historiskt sett fanns debuggers inte långt efter att de första programmen gick snett, vilket gav utvecklarna medel för att fixa sin kod utan att famla i mörkret.

Med Lua förlitar du dig ofta på externa debuggers eller bygger in dem i din utvecklingsmiljö. ZeroBrane Studio, till exempel, är en IDE som helt integrerar en Lua-debugger. Den låter dig stega igenom kod, sätta brytpunkter och övervaka variabler. På implementeringssidan använder debuggers vanligen krokar för att infoga brytpunkter och andra felsökningsfaciliteter.

Alternativ? Absolut. Gamla goda `print` uttalanden, kärt känd som "printf debugging", kan ibland göra tricket utan avancerade verktyg.

## Se även
För att fortsätta din felsökningsresa, kolla in:

- ZeroBrane Studio: https://studio.zerobrane.com/
- Lua-users wiki om felsökning av Lua-kod: http://lua-users.org/wiki/DebuggingLuaCode
- Referensen för `debug` biblioteket i Lua's manual: https://www.lua.org/manual/5.4/manual.html#6.10
