---
title:                "Feilhåndtering"
aliases: - /no/lua/handling-errors.md
date:                  2024-01-26T00:55:46.758824-07:00
model:                 gpt-4-1106-preview
simple_title:         "Feilhåndtering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/handling-errors.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å håndtere feil i koding handler om å forvente det uventede. Det er kunsten å planlegge for når ting går galt, slik at du kan holde programmet ditt i jevn drift.

## Hvordan:
Lua bruker to hovedfunksjoner for feilhåndtering: `pcall` og `xpcall`. Slik bruker du dem:

```lua
function might_fail()
    if math.random() > 0.5 then
        error("Oi! Noe gikk galt.")
    else
        print("Alt bra!")
    end
end

-- Bruker pcall
local success, errorMessage = pcall(might_fail)

if success then
    print("Suksess!")
else
    print("Fanget en feil:", errorMessage)
end

-- Bruker xpcall med en feilhåndterer
function myErrorHandler(err)
    print("Feilhåndterer sier:", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("Var kallet vellykket?", status)
```

Et eksempel på utskrift kan være:

```
Fanget en feil: Oi! Noe gikk galt.
Feilhåndterer sier: Oi! Noe gikk galt.
Var kallet vellykket? false
```
Eller, hvis ingen feil oppstår:
```
Alt bra!
Suksess!
Alt bra!
Var kallet vellykket? true
```

## Dypdykk
Å håndtere feil, eller "unntakshåndtering", var ikke alltid en greie. Tidlige programmer krasjet – mye. Ettersom kodingen utviklet seg, så gjorde også behovet for stabilitet. Lua sin tilnærming er enkel sammenlignet med noen språk. Det finnes ingen `try/catch` blokker, bare `pcall` og `xpcall`. Den førstnevnte beskytter et funksjonskall, og returnerer en status og eventuelle feil. Den sistnevnte legger til en feilhåndteringsfunksjon, nyttig for tilpasset opprydning eller logging.

Et alternativ i Lua er å bruke `assert`, som kan tjene et lignende formål ved å kaste en feil hvis dens betingelse er falsk. Men det er ikke så fleksibelt som `pcall` for komplekse feilhåndteringsscenarier.

Internt fungerer `pcall` og `xpcall` ved å sette opp et "beskyttet miljø" for funksjonen å kjøre. Hvis en feil dukker opp, fanger miljøet den og kan enten håndtere den med en gang eller sende den tilbake for at programmet skal håndtere den.

## Se Også
- Boken "Programming in Lua" (tredje utgave), tilgjengelig på https://www.lua.org/pil/ for grundig lesing om feilhåndtering (Seksjon 8.4).
- Offisielle Lua 5.4 Referansehåndbok: https://www.lua.org/manual/5.4/ - for den mest oppdaterte informasjonen om Lua sine feilhåndteringsfunksjoner.
- Lua-brukeres wiki om feilhåndtering: http://lua-users.org/wiki/ErrorHandling – for fellesskapets innsikt og mønstre.
