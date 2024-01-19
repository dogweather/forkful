---
title:                "Genererer tilfeldige tall"
html_title:           "PHP: Genererer tilfeldige tall"
simple_title:         "Genererer tilfeldige tall"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall er prosessen for å lage tall som ikke kan forutsies bedre enn ved tilfeldig sjanse. Programmerere gjør dette for å simulere uforutsigbarhet i deres applikasjoner eller spill.

## Hvordan gjør vi det?

Her er et enkelt eksempel på hvordan du kan generere tilfeldige tall i Lua:

```Lua
math.randomseed(os.time())

for i = 1, 5 do
    print(math.random(50))
end
```

Dette programmet vil generere og skrive ut fem tilfeldige tall mellom 1 og 50. Siden vi frøs randomizeren med gjeldende tidsstempel, vil tallene endre seg hver gang du kjører programmet.

## Dypdykk

Generering av tilfeldige tall har en lang historie i databehandling og er sentral for mange applikasjoner, inkludert kryptografi og simuleringer. I Lua kan du også generere tall mellom to verdier ved hjelp av `math.random(lower, upper)`. Dette kan være nyttig for å få tilfeldige tall i et begrenset område.

Et annet alternativ er å bruke Lua's innebygde `math.random()` funksjon uten noen argumenter. Dette returnerer et flyttall mellom 0 og 1, noe som er nyttig for mange simuleringer og beregninger.

Det skal bemerkes at Lua's innebygde randum funksjoner ikke er kryptografisk sikre. Hvis du trenger tilfeldige tall for kryptografi eller sikkerhetsrelatert arbeid, bør du se på kraftigere alternativer som OpenSSL eller andre tredjepartsbiblioteker.

## Se også

1. Lua's matematikkbibliotek dokumentasjon: https://www.lua.org/pil/18.html 
2. Kryptografisk sikre tilfeldige tall i Lua: https://love2d.org/wiki/Randomness 
3. Et dypt dykk inn i Random Number Generation (RNG): https://www.oreilly.com/library/view/beautiful-visualization/9781449309869/ch09.html 
4. Guide til simuleringer og tilfeldige tall i Lua: https://www.tutorialspoint.com/lua/lua_random_numbers.htm