---
date: 2024-01-20 17:40:51.161685-07:00
description: "Hur man g\xF6r: I \xE4ldre versioner av vissa operativsystem skapade\
  \ `os.tmpname()` enbart ett namn, inte sj\xE4lva filen. Nu skapar de flesta tempor\xE4\
  ra filer\u2026"
lastmod: '2024-04-05T22:50:52.362434-06:00'
model: gpt-4-1106-preview
summary: "I \xE4ldre versioner av vissa operativsystem skapade `os.tmpname()` enbart\
  \ ett namn, inte sj\xE4lva filen."
title: "Skapa en tempor\xE4r fil"
weight: 21
---

## Hur man gör:
```Lua
local os = require("os")

-- Skapa en tillfällig fil
local temp_filename = os.tmpname()
print("Tillfällig fil skapad:", temp_filename)

-- Använd filen, skriver exempeltext
local temp_file = io.open(temp_filename, "w")
temp_file:write("Det här är en text i en tillfällig fil.")
temp_file:close()

-- Läs från den tillfälliga filen
temp_file = io.open(temp_filename, "r")
print("Filinnehåll:", temp_file:read("*a"))
temp_file:close()

-- Ta bort den tillfälliga filen när den inte längre behövs
os.remove(temp_filename)
print("Tillfällig fil borttagen:", temp_filename)
```
Resultat:
```
Tillfällig fil skapad: /tmp/lua_AxB72
Filinnehåll: Det här är en text i en tillfällig fil.
Tillfällig fil borttagen: /tmp/lua_AxB72
```

## Djupdykning:
I äldre versioner av vissa operativsystem skapade `os.tmpname()` enbart ett namn, inte själva filen. Nu skapar de flesta temporära filer säkert. Viktigt att tänka på är dock tillgången och rättigheterna för temp-foldern. Alternativ kan vara att använda externa bibliotek som `luafilesystem` för mer kontroll. När du skapar en tillfällig fil, undvik kollisioner och säkerhetsrisker genom att låta systemet hantera namngivningen.

## Se även:
- Lua FileSystem (lfs): https://keplerproject.github.io/luafilesystem/
- Lua 5.4 Reference Manual: https://www.lua.org/manual/5.4/manual.html
- Säkerhetsaspekter av temporära filer: https://owasp.org/www-community/vulnerabilities/Insecure_Temporary_File
