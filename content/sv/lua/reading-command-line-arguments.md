---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad och varför?

Kommandoradsargument är data som skickas in i ett program när det startas. Används i programmering för att styra programmet i en viss riktning, som att ange en filväg för filbearbetning eller att kontrollera startinställningar.

## Så här gör du:

I Lua kan du läsa kommandoradsargument genom globala arrayen `arg`. Ditt första argument, som är programnamnet, finns på indexet `arg[0]`. Följande argument finns på `arg[1]`, `arg[2]` och så vidare.

```Lua
print("Namn på programmet: ", arg[0])
for i = 1, #arg do
  print("Argument ", i, " är ", arg[i])
end
```

Om vi kör programmet med följande kommandorad: `lua myfile.lua hej världen`, output blir:

```Lua
Namn på programmet:  myfile.lua
Argument 1 är hej
Argument 2 är världen
```
## Djupdykning

Kommandoradsargument har använts i programmeringen sedan de tidigaste dagarna när terminalbaserade användargränssnitt var normen. I Lua, `arg`-tabellen, inte bara lagrar argumenten men också erbjuder några unika index: `arg[-1]` ger vilken tolk som används, och `arg[n]`, där n är större än antalet argument, returnerar `nil`.

Ett alternativ till `arg` är `io.read()`, som läser input under körning av programmet snaraste än vid start. Dock kan detta göra scriptet mindre användarvänligt och öppet för fel om användaren inte är medveten om att input behövs.

## Se också:

Mer hjälp om att använda kommandoradsargument i Lua finns på följande platser:
1. Lua-Användar-Wiki: Kommandoradsargument: https://lua-users.org/wiki/CommandLineProcessing
2. StackOverflow: Hur man hanterar kommandoradsargument i Lua: https://stackoverflow.com/questions/10386976/how-to-handle-command-line-arguments-in-lua
3. Officiell Lua 5.3 Manual: https://www.lua.org/manual/5.3/