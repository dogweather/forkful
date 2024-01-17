---
title:                "Interpolering av en streng"
html_title:           "Haskell: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Interpolering av strenger er prosessen med å sette inn variabler eller verdier i en streng. Dette gjøres for å bygge mer dynamiske og fleksible strenger som kan tilpasses ulike situasjoner. Mange programmerere bruker denne teknikken for å gjøre koden sin mer intuitiv og lesbar.

# Hvordan:
Her er et eksempel på bruk av interpolering i Haskell:

```Haskell
-- Definerer en funksjon som tar inn navn og alder og returnerer en setning med informasjonen
introduksjon :: String -> Int -> String
introduksjon navn alder = "Hei, mitt navn er " ++ navn ++ " og jeg er " ++ show alder ++ " år gammel."

-- Kaller funksjonen med parameterverdiene "Lise" og 25
introduksjon "Lise" 25
```

Output:

```
Hei, mitt navn er Lise og jeg er 25 år gammel.
```

Her bruker vi ```++``` operatoren til å kombinere de statiske delene av setningen med verdiene av parameterene som blir gitt når funksjonen kalles. Vi bruker også funksjonen ```show``` for å konvertere Integer alderen til en String slik at den kan settes inn i setningen.

# Dypdykk:
Interpolering av strenger har blitt en vanlig teknikk i mange programmeringsspråk, inkludert Haskell. Det er en effektiv måte å lage mer dynamiske og leselige strenger på. Før denne teknikken ble vanlig, måtte programmere bruke mer komplekse metoder for å samle en setning med variabler og verdiene til disse variablene.

Et alternativ til interpolering er bruk av funksjoner som ```printf``` og ```format``` som tillater programmerere å spesifisere hvordan verdiene skal settes inn i en setning. Dette er vanligvis brukt i språk som C og Python.

I Haskell bruker man i stedet funksjoner som ```show``` for å konvertere verdien av en variabel til en String som kan brukes i en interpolert streng. Dette kan virke litt mer tungvint, men i Haskell er det vanligvis bedre å ha klart definerte funksjoner som gjør spesifikke oppgaver, i stedet for å ha mer generelle funksjoner som kan føre til forvirrende kodesnutter.

# Se også:
- [Haskell dokumentasjon om String Interpolasjon](https://wiki.haskell.org/String_interpolation)
- [En sammenligning av ulike metoder for å sette inn verdier i strenger](https://www.safaribooksonline.com/blog/2011/09/29/c-comparing-printf-to-user-defined-formatters-in-c/)