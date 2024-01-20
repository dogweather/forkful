---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

# Lua (den aktuella versionen) - Skriv ut felsökningsoutput

## Vad & Varför?

Att skriva ut felsökningsoutput, eller "debugging", är när du visar programmeringsdata för att spåra och fixa kodfel. Programmerare gör detta för att lättare identifiera fel och problemområden inom koden.

## Så här gör du:

I Lua skriver du ut med `print()` funktionen. Här är hur du gör:

```Lua
print("Hej, världen!")  -- skriver ut "Hej, världen!"
```
Vill du skriva ut variabler? Inga problem, så här:

```Lua
namn = "Anna"
print(namn)  -- skriver ut "Anna"
```
Men säg att du vill spåra en bugg i en loop:

```Lua
for i = 1, 5 do
   print("Loop nummer: " .. i) -- skriver ut "Loop nummer: 1", "Loop nummer: 2", etc.
end
```

## Djupdykning

Historiskt sett har alla programmeringsspråk haft någon form av utskriftsfunktion för felsökning. I Lua har vi `print()` funktionen, men det finns alternativ. Du kan t.ex. använda `io.write()` för att skriva ut utan radbyte, eller skapa dina egna felsökningsfunktioner.

```Lua
io.write("Hej, ") io.write("värden!")  -- skriver ut "Hej, värden!"
```
Kom ihåg att `print()` i Lua egentligen bara är en inbyggd wrapper runt `io.write()`. Den lägger till ett radbyte och omvandlar variabler till strängar åt dig.

## Se också

För en djupare förståelse av felsökning i Lua, se dessa resurser:

- [Programming in Lua: Input and Output](http://www.lua.org/pil/21.1.html)
- [Lua-users Wiki: Simple Stand Alone Debug Info](http://lua-users.org/wiki/SimpleStandAloneDebugInfo)
- [Dev.To: Debugging in Lua - A deep dive](https://dev.to/nabbisen/debugging-in-lua-a-deep-dive-3m3f)