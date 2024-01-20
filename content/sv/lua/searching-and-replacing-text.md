---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Vad och Varför?

Sökning och ersättning av text är processen att hitta specifika strängsekvenser i en text och byta ut dem med något annat. Programmerare gör detta för att enkelt modifiera data, korrigera fel eller för att standardisera text.

# Hur:

Här är hur Lua hanterar en sök- och ersätt-operation genom `string.gsub` funktionen.

```Lua
s = "Hello, World!"
s = string.gsub(s, "World", "Sweden")
print(s)
```
Resultat:

```
Hello, Sweden!
```

# Djupdykning

Lua har använt sök och ersätt-tekniker sedan den första versionen, som en del av dess stränghanteringsfunktioner. Alternativ till `string.gsub` inkluderar skapande av egna funktioner för komplexa sökningar och ersättningar, och även genom att använda 'LPEG' (Lua Parsing Expression Grammar) för mer utmanande strömmar av karaktärer.

Ett av det unika med `string.gsub` är att det kan ta in en funktion som sitt tredje argument. Denna funktion kallas för varje matchning, och det returnerade värdet används som ersättning.

```Lua
s = "1000"
s = string.gsub(s, "(%d)", function(d) return tonumber(d) * 10 end)
print(s)
```
Resultat:

```
10100
```

# Se Även

För mer detaljerad information och ytterligare koncept, se dessa resurser:

- [Programming in Lua : 20 - The String Library](http://www.lua.org/pil/20.html)
- [Lua-Users Wiki : String Library Tutorial](http://lua-users.org/wiki/StringLibraryTutorial)
- [Stack Overflow - How to do a regular expression replace in Lua?](https://stackoverflow.com/questions/15909669/how-to-do-a-regular-expression-replace-in-lua)