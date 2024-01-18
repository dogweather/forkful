---
title:                "Arbeta med json"
html_title:           "Lua: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/working-with-json.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att arbeta med JSON är ett sätt för programmerare att hantera och utbyta data på ett strukturerat och läsbar sätt. Det är ett vanligt format som används för att lagra och överföra data, och har blivit alltmer populärt inom webbutveckling. JSON är enkelt att läsa och tolka, vilket gör det till ett användbart verktyg för att integrera och kommunicera med olika system och plattformar.

## Så här:
Att arbeta med JSON i Lua är enkelt. Det finns flera bibliotek som du kan använda för att konvertera dina data till JSON-format och vice versa. Här är ett exempel på hur man kan skapa och skriva ut en JSON-sträng i Lua:

```lua
-- Skapa ett Lua-objekt med några data
local movie = {
  title = "Jurassic Park",
  release_year = 1993,
  director = "Steven Spielberg",
  actors = { "Sam Neill", "Laura Dern", "Jeff Goldblum" },
  awards = {
    Academy_Award = true,
    BAFTA = true,
    Golden_Globe = false
  }
}

-- Importera json-biblioteket
local json = require("json")

-- Konvertera objektet till en JSON-sträng
local movie_json = json.encode(movie)

-- Skriv ut den resulterande strängen
print(movie_json)
```

Output:

```bash
{"actors":["Sam Neill","Laura Dern","Jeff Goldblum"],"awards":{"Academy_Award":true,"BAFTA":true,"Golden_Globe":false},"director":"Steven Spielberg","release_year":1993,"title":"Jurassic Park"}
```

För att konvertera en JSON-sträng till ett Lua-objekt kan du använda funktionen `json.decode()`:

```lua
-- En JSON-sträng som ska konverteras
local json_str = '{"name": "John Doe", "age": 25, "hobbies": ["gaming", "reading", "movies"]}'

-- Konvertera strängen till ett Lua-objekt
local user = json.decode(json_str)

-- Hämta data från objektet
print(user.name)  -- Output: John Doe
print(user.age)   -- Output: 25
print(user.hobbies[2])  -- Output: reading
```

## Djupdykning:
JSON (JavaScript Object Notation) utvecklades ursprungligen av Douglas Crockford på 1990-talet och användes främst för att lagra data i JavaScript-applikationer. Det har sedan dess blivit ett standardformat för att strukturera och utbyta data inom webbapplikationer. 

Alternativ till JSON inkluderar XML och YAML, men JSON har blivit mer populärt på grund av sin enklare syntax och snabbare parsing. Det finns också olika sätt att implementera JSON i Lua, inklusive bibliotek som cjson och dkjson.

## Se även:
- [Lua json-bibliotek](http://regex.info/blog/lua/json)
- [JSON.org](https://www.json.org/json-en.html)
- [Lua-programmering på 30 minuter](https://tylerneylon.com/a/learn-lua/)