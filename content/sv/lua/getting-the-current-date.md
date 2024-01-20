---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?

 Att få det aktuella datumet innebär att hämta den rådande datuminformationen från systemet. Programmerare gör detta för att hålla reda på händelser, logga data, eller för att hantera tidsberoende funktioner.

## Så här gör du:

För att få det nuvarande datumet i Lua används os.date funktionen. Här är ett exempel:

```Lua
aktuell_datum = os.date("%d/%m/%Y")
print(aktuell_datum)
```

Om du kör detta kodstycke kommer det att skriva ut det aktuella datumet i formatet DD/MM/ÅÅÅÅ.

## Djupdykning

Historiskt sett har datumhantering i programmering varit knepigt, men moderna språk som Lua har inbyggda standardbibliotek som förvaltar tid och datum.

Alternativt till os.date, kan os.time också användas för att få nuvarande Unix tidstämpel, vilket är antalet sekunder sedan 1970. Detta ger ett enklare sätt att mäta tid. 

```Lua
unix_tid = os.time()
print(unix_tid)
```

Tidsfunktionerna i Lua är baserade på de POSIX-standarder som finns på de flesta operativsystem, men kan ha skillnader i icke-POSIX miljöer.

## Se även

1. [Lua 5.4 Referensmanual - os.date](https://www.lua.org/manual/5.4/manual.html#6.9)
2. [Lua Användar-Wiki - Datum och Tid](http://lua-users.org/wiki/DateAndTime)
   
Kom ihåg, att det är avgörande att hantera tid och datum korrekt i din programvara. Lycka till med programmeringen!