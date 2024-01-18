---
title:                "Opprettelse av en midlertidig fil"
html_title:           "Lua: Opprettelse av en midlertidig fil"
simple_title:         "Opprettelse av en midlertidig fil"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Opprette en midlertidig fil er en vanlig praksis blant programmører. Det er en måte å midlertidig lagre data eller informasjon på, som skal brukes senere i koden. Dette kan være nyttig når man trenger å behandle en stor mengde data eller når man trenger å sende informasjon til en annen del av koden.

## Slik gjør du det:
```Lua
--create a temporary file
local tmp_file = io.tmpfile()

--write data to the file
tmp_file:write("This is some sample data")

--read from the file
local data = tmp_file:read("*all")

--close the file
tmp_file:close()

--print the data
print(data) --output: This is some sample data
```

## Dypdykk:
Opprettelsen av midlertidige filer har sin historie fra tiden da de fleste datamaskiner hadde begrenset lagringsplass. Det var vanlig å opprette en midlertidig fil for å lagre data, for deretter å slette den når den ikke lenger var nødvendig. Alternativene til å opprette en midlertidig fil er å bruke variabler eller å lagre data i minnet. Implementeringen av å opprette en midlertidig fil i Lua er enkel og kan gjøres ved å bruke den innebygde funksjonen io.tmpfile().

## Se også:
https://www.lua.org/manual/5.4/manual.html#pdf-io.tmpfile