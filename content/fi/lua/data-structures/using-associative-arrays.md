---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:26.930476-07:00
description: "Miten: Luassa assosiatiivisen taulukon (tai Luassa puhuttuna taulun)\
  \ luominen on suoraviivaista. Hylk\xE4\xE4t tavanomaiset numeeriset indeksit ja\
  \ valitset omat\u2026"
lastmod: '2024-03-13T22:44:56.690851-06:00'
model: gpt-4-0125-preview
summary: Luassa assosiatiivisen taulukon (tai Luassa puhuttuna taulun) luominen on
  suoraviivaista.
title: "Assosiatiivisten taulukoiden k\xE4ytt\xF6"
weight: 15
---

## Miten:
Luassa assosiatiivisen taulukon (tai Luassa puhuttuna taulun) luominen on suoraviivaista. Hylkäät tavanomaiset numeeriset indeksit ja valitset omat avaimet. Katso tämä:

```Lua
-- Assosiatiivisen taulukon luominen
userInfo = {
  name = "Jamie",
  occupation = "Seikkailija",
  level = 42
}

-- Elementtien käyttäminen
print(userInfo["name"]) -- Tulostaa Jamie
print(userInfo.occupation) -- Tulostaa Seikkailija

-- Uusien avain-arvo -parien lisääminen
userInfo["hobby"] = "Koodaus"
userInfo.favLang = "Lua"

-- Assosiatiivisen taulukon iteroiminen
for key, value in pairs(userInfo) do
  print(key .. ": " .. value)
end
```

Tuloste:
```
Jamie
Seikkailija
name: Jamie
occupation: Seikkailija
level: 42
hobby: Koodaus
favLang: Lua
```

Siistiä, eikö? Käyt interactiivisesti datan kanssa avaimilla, jotka ovat merkityksellisiä sinulle, mikä tekee koodista luettavampaa ja ylläpidettävämpää.

## Syväsukellus
Kun Lua tuli näyttämölle, se esitteli taulut kaiken kattavana datastruktuurina, mullistaen sen, miten kehittäjät hallinnoivat dataa. Toisin kuin joissakin kielissä, joissa assosiatiiviset taulukot ja taulukot ovat erillisiä entiteettejä, Luassa taulut toimivat sekä tauluina että assosiatiivisina taulukoina, yksinkertaistaen datastruktuurimaisemaa.

Mikä tekee Luasta tauluista erityisen tehokkaita, on niiden joustavuus. Kuitenkin, tämä joustavuus tulee potentiaalisen suorituskykyvaikutuksen kustannuksella, erityisesti suurilla datamäärillä, joissa erikoistuneempi datastruktuuri voisi olla tehokkaampi valinta.

Vaikka Lua ei natiivisti tue enemmän perinteisiä datastruktuureita suoraan laatikosta, kuten linkitettyjä listoja tai hajautustaulukkoja, taulurakenteen sopeutuvuus tarkoittaa, että voit toteuttaa nämä käyttäen tauluja, jos tarvitset. Muista vain: suuren voiman myötä tulee suuri vastuu. Käytä joustavuutta viisaasti ylläpitääksesi koodisi suorituskykyä ja luettavuutta.
