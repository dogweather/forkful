---
title:                "Generering av tilfeldige tall"
html_title:           "Lua: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Generering av tilfeldige tall er en viktig del av mange programmeringsprosjekter. Dette refererer til prosessen med å skape tall som ikke følger et forutbestemt mønster. Programmerere gjør dette for å legge til variasjon og usikkerhet i sine programmer, for eksempel for å simulere tilfeldige hendelser eller for å lage krypteringsnøkler.

## Slik gjør du:
I Lua kan du generere tilfeldige tall ved hjelp av funksjonen ```math.random()```. Denne funksjonen tar inn to tall som argumenter, og vil deretter returnere et tilfeldig tall mellom disse to tallene. Her er et eksempel på hvordan du kan bruke denne funksjonen:
```Lua
-- Genererer et tilfeldig tall mellom 1 og 10
print(math.random(1, 10)) 
-- Output: 7 

-- Genererer et tilfeldig desimaltall mellom 0 og 1
print(math.random())
-- Output: 0.508761894524 
```

## Dypdykk: 
Tilfeldig tallgenerering har blitt brukt i programmering siden begynnelsen av datamaskinteknologi. Før det var vanlig å bruke fysiske fenomener som radioaktivt nedbrytning eller slumping av en roulettrulle for å generere tilfeldige tall. Alternativt kan programmerere også bruke en tilfeldighetsgenerator som en del av språket de programmerer i, for eksempel ```random``` modulen i Python.

Lua bruker en algoritme som kalles "Linear Congruential Generator" for å generere tilfeldige tall. Dette er en metode for å produsere en sekvens av tall som ser ut som tilfeldige, men faktisk følger et forutbestemt mønster. Derfor er det viktig å huske at tilfeldige tall i Lua egentlig ikke er helt tilfeldige, og de skal ikke brukes til krypteringsformål.

## Se også: 
- Lua dokumentasjon for ```math.random()``` funksjonen 
(https://www.lua.org/manual/5.3/manual.html#pdf-math.random) 
- En artikkel om tilfeldige tallgenerering og dens bruksområder i programmering 
(https://www.geeksforgeeks.org/pseudo-random-number-generator/)