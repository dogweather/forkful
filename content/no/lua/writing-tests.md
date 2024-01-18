---
title:                "Skriving av tester"
html_title:           "Lua: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Skriving av tester er en viktig del av å være en programmerer. Det referer til prosessen med å lage små biter av kode som sjekker om koden du allerede har skrevet fungerer riktig. Dette er viktig for å sikre at din kode er pålitelig og fungere som den skal.

## Hvordan skal du:

Å skrive tester i Lua er ganske enkelt. La oss se på et eksempel:

```Lua
-- Definere en funksjon
function add(x, y)
  return x + y
end

-- Skrive en test
assert(add(3, 4) == 7, "Addisjons test feilet!")

-- Viser melding når testen er bestått
print("Alle tester gikk bra!")
```
Output:
```txt 
Alle tester gikk bra!
```

I dette eksempelet har vi opprettet en funksjon som tar to tall og legger dem sammen. Vi har da skrevet en test som sjekker om funksjonen fungerer som den skal ved å legge sammen tallene 3 og 4. Hvis testen feiler, vil en feilmelding bli vist. Hvis den går gjennom uten å feile, vil vi få beskjed om at alle tester gikk bra.

Du kan også skrive mer avanserte tester ved å bruke Lua's unit-testing bibliotek, som tillater deg å organisere flere tester og sjekke flere forhold.

## Dypdykk

Konseptet med å skrive tester har eksistert lenge og har blitt ansett som en god praksis innen programmering. Det hjelper programmører med å identifisere og fikse feil tidlig i utviklingsprosessen og sikrer at koden fungerer som den skal.

Alternativt kan noen programmører bruke "print"-setninger for å sjekke for feil istedenfor å skrive tester. Dette er ikke en god praksis og kan være tidkrevende å finne feil og fikse dem senere.

Når du skriver tester i Lua, er det viktig å vite at testkoden ikke skal være en del av den endelige koden din. Tester er kun for å sjekke eksisterende kode, og bør ikke påvirke den.

## Se også:

- Lua's offisielle dokumentasjon for testing: https://www.lua.org/pil/15.html
- En guide til enhetstesting i Lua: https://cloudcraft.info/unit-testing-in-lua/
- En diskusjon om fordeler med å skrive tester i programmering: https://medium.freecodecamp.org/the-importance-of-writing-tests-for-programmers-38e4ba6be548