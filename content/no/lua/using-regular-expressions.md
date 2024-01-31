---
title:                "Bruk av regulære uttrykk"
date:                  2024-01-19
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk er mønstre for å matche tekststrenger. Programmerere bruker dem for å søke, erstatte, og validere tekst på en kjapp og fleksibel måte.

## Hvordan:
```Lua
-- Søker etter ordet "Norge" i en tekststreng
local tekst = "Jeg elsker Norge!"
local funnet = string.match(tekst, "Norge")
print(funnet) -- Output: Norge

-- Erstatter "blå" med "rød" i en tekststreng
local fargetekst = "Himmelen er blå."
local byttetFarge = string.gsub(fargetekst, "blå", "rød")
print(byttetFarge) -- Output: Himmelen er rød.

-- Validerer et enkelt e-post format
local email = "ola@nordmann.no"
local match = string.match(email, "[%w._%-%+]+@[%w._%-]+%.%w+")
print(match == email) -- Output: true
```

## Dykk Ned:
Historisk sett kommer regulære uttrykk fra teoretisk informatikk og var blant de første automatiseringsverktøyene i databehandling. I Lua er det begrensede, innebygde støtten sammenlignet med språk som Perl eller Python. Man kan bruke biblioteker som `lpeg` eller `rex` for mer komplekse behov. Lua bruker egne mønstre som er lik, men ikke identisk med standard POSIX- eller Perl-kompatible regulære uttrykk.

## Se Også:
- [Stack Overflow: Lua Pattern Matching vs. Regular Expressions](https://stackoverflow.com/questions/2925159/lua-pattern-matching-vs-regular-expressions)
