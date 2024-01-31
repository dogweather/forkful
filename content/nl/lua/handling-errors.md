---
title:                "Fouten afhandelen"
date:                  2024-01-28T22:02:03.600637-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"

category:             "Lua"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/lua/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Fouten afhandelen in programmeren gaat over het verwachten van het onverwachte. Het is de kunst van het plannen voor wanneer dingen misgaan, zodat je programma soepel kan blijven draaien.

## Hoe te:
Lua gebruikt twee belangrijke functies voor foutafhandeling: `pcall` en `xpcall`. Dit is hoe je ze gebruikt:

```lua
function might_fail()
    if math.random() > 0.5 then
        error("Oeps! Er is iets misgegaan.")
    else
        print("Alles goed!")
    end
end

-- Gebruik van pcall
local success, errorMessage = pcall(might_fail)

if success then
    print("Succes!")
else
    print("Een fout gevangen:", errorMessage)
end

-- Gebruik van xpcall met een foutafhandelaar
function myErrorHandler(err)
    print("Foutafhandelaar zegt:", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("Was de aanroep succesvol?", status)
```

Voorbeelduitvoer kan zijn:

```
Een fout gevangen: Oeps! Er is iets misgegaan.
Foutafhandelaar zegt: Oeps! Er is iets misgegaan.
Was de aanroep succesvol? false
```
Of, als er geen fout optreedt:
```
Alles goed!
Succes!
Alles goed!
Was de aanroep succesvol? true
```

## Diepgaande Duik
Fouten afhandelen, of "uitzonderingsbehandeling", was niet altijd iets. Vroege programma's crashten – veel. Naarmate programmeren evolueerde, deed de behoefte aan stabiliteit dat ook. Lua's benadering is simpel vergeleken met sommige talen. Er zijn geen `try/catch` blokken, gewoon `pcall` en `xpcall`. De eerste beschermt een functie-aanroep, retourneert een status en eventuele fout. De laatste voegt een foutafhandelingsfunctie toe, nuttig voor aangepaste opruiming of loggen.

Een alternatief in Lua is om `assert` te gebruiken, dat een soortgelijk doel kan dienen door een fout te gooien als de voorwaarde onwaar is. Maar het is niet zo flexibel als `pcall` voor complexe foutafhandelingssituaties.

Intern werken `pcall` en `xpcall` door het opzetten van een "beschermde omgeving" voor de functie om in te draaien. Als er een fout opduikt, vangt de omgeving deze op en kan deze direct afhandelen of teruggeven aan het programma om te behandelen.

## Zie Ook
- Het boek Programming in Lua (derde editie), beschikbaar op https://www.lua.org/pil/ voor grondige lectuur over foutafhandeling (Sectie 8.4).
- Officiële Lua 5.4 Referentiehandleiding: https://www.lua.org/manual/5.4/ - voor de meest actuele info over Lua's foutafhandelingsfuncties.
- Lua-gebruikers wiki over foutafhandeling: http://lua-users.org/wiki/ErrorHandling – voor inzichten en patronen van de gemeenschap.
