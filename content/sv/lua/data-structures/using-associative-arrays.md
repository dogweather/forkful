---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:23.113379-07:00
description: "Hur man g\xF6r: I Lua \xE4r det rakt p\xE5 sak att skapa en associerad\
  \ array (eller en tabell, p\xE5 Lua-spr\xE5k) . Du skippar de vanliga numeriska\
  \ indexerna f\xF6r\u2026"
lastmod: '2024-04-05T22:37:46.728301-06:00'
model: gpt-4-0125-preview
summary: "I Lua \xE4r det rakt p\xE5 sak att skapa en associerad array (eller en tabell,\
  \ p\xE5 Lua-spr\xE5k) ."
title: "Att anv\xE4nda associativa arrayer"
weight: 15
---

## Hur man gör:
I Lua är det rakt på sak att skapa en associerad array (eller en tabell, på Lua-språk) . Du skippar de vanliga numeriska indexerna för nycklar som du själv väljer. Kolla in detta:

```Lua
-- Skapar en associerad array
userInfo = {
  namn = "Jamie",
  yrke = "Äventyrare",
  nivå = 42
}

-- Åtkomst av element
print(userInfo["namn"]) -- Skriver ut Jamie
print(userInfo.yrke) -- Skriver ut Äventyrare

-- Lägger till nya nyckel-värde-par
userInfo["hobby"] = "Kodning"
userInfo.favSpråk = "Lua"

-- Itererar över den associerade arrayen
for nyckel, värde in pairs(userInfo) do
  print(nyckel .. ": " .. värde)
end
```

Output:
```
Jamie
Äventyrare
namn: Jamie
yrke: Äventyrare
nivå: 42
hobby: Kodning
favSpråk: Lua
```

Det coola? Du interagerar med data med nycklar som är meningsfulla för dig, vilket gör koden mer läsbar och underhållbar.

## Djupdykning
När Lua gjorde entré på scenen introducerade det tabeller som en allt-i-ett-datatyp, vilket revolutionerade hur utvecklare hanterar data. Till skillnad från i vissa språk där associerade arrayer och arrayer är distinkta enheter, fungerar Luas tabeller som både och, vilket förenklar datatypslanskapet.

Det som gör Lua-tabeller särskilt kraftfulla är deras flexibilitet. Dock kommer denna flexibilitet med en potentiell prestandapåverkan, särskilt med stora dataset där en mer specialiserad datatyp kan vara att föredra för effektivitet.

Även om Lua inte nativt stöder mer konventionella datatyper direkt ur lådan, såsom länkade listor eller hashkartor, så betyder tabellstrukturens anpassningsbarhet att du kan implementera dessa med tabeller om du behöver. Kom bara ihåg: med stor kraft följer stort ansvar. Använd flexibiliteten klokt för att bibehålla prestanda och läsbarhet i din kod.
