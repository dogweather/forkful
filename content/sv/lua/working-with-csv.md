---
title:                "Arbeta med csv"
html_title:           "Lua: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med CSV (Comma Separated Values) betyder att man hanterar filer eller data där informationen är separerad med kommatecken. Detta gör det enkelt att läsa och dela information med andra system. Programmers använder CSV för att lagra och överföra data på ett strukturerat sätt, vilket underlättar bearbetning och analys av stora datamängder.

## Hur man gör:
Här är ett enkelt exempel på hur man kan läsa in en CSV-fil och skriva ut innehållet i en tabell i Lua-programmering:
```
-- Läser in filen som en textsträng
local file = io.open("exempel.csv", "r")
local content = file:read("*all")
file:close()

-- Delar upp innehållet utifrån kommatecken
local rows = {}
for row in content:gmatch("[^,\r\n]+") do 
  rows[#rows+1] = row 
end 

-- Skriver ut varje rad i en tabell
for i = 1, #rows do 
  print(rows[i]) 
end
```

Output:
```
Förnamn,Efternamn,Ålder
Lisa,Larsson,25
Erik,Jansson,36
Anna,Nilsson,42 
```

## Djupdykning:
CSV är ett vanligt format för att lagra och överföra data och har funnits sedan 1972. Ett alternativ till CSV är JSON (JavaScript Object Notation), som har blivit allt mer populärt på senare år. CSV-filer kan också ha olika tecken för att separera data, såsom semikolon eller tabb, beroende på vilket system som använder filen. Det är viktigt att ha detta i åtanke vid hantering av CSV-filer för att undvika datafel.

## Se även:
- [Lua Reference Manual](https://www.lua.org/manual/5.3/)
- [Komma igång med Lua](https://www.lua.org/start.html)
- [CSV i Lua](https://riptutorial.com/lua/example/23265/csv-in-lua)