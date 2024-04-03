---
date: 2024-01-26 03:45:20.356222-07:00
description: "Kuinka: Fishiss\xE4 numeroiden py\xF6rist\xE4minen perustuu `math`-komentoon.\
  \ K\xE4yt\xE4 `math -s0` py\xF6rist\xE4\xE4ksesi l\xE4himp\xE4\xE4n kokonaislukuun."
lastmod: '2024-03-13T22:44:56.987117-06:00'
model: gpt-4-0125-preview
summary: "Fishiss\xE4 numeroiden py\xF6rist\xE4minen perustuu `math`-komentoon."
title: "Numerojen py\xF6rist\xE4minen"
weight: 13
---

## Kuinka:
Fishissä numeroiden pyöristäminen perustuu `math`-komentoon. Käytä `math -s0` pyöristääksesi lähimpään kokonaislukuun.

```fish
# Pyöristä ylöspäin
echo (math -s0 "4.7")
# Tuloste: 5

# Pyöristä alaspäin
echo (math -s0 "4.3")
# Tuloste: 4

# Pyöristä kahden desimaalin tarkkuuteen
echo (math -s2 "4.5678")
# Tuloste: 4.57

# Pyöristä negatiivinen numero
echo (math -s0 "-2.5")
# Tuloste: -3
```

## Syväsukellus
Historiallisesti numeroiden pyöristäminen tehtiin manuaalisemmin tai ulkoisilla työkaluilla, mutta modernissa kuorissa, kuten Fishissä, se on sisäänrakennettu työkaluihin. Fishin lähestymistapa `math`-komennon käytössä yksinkertaistaa asioita verrattuna vanhempiin kuoriin. Vaihtoehdot muissa ohjelmointiympäristöissä vaihtelevat; kielet kuten Python käyttävät funktioita kuten `round()`, kun taas Bash saattaa vaatia monimutkaisempia ilmaisuja tai `bc`-työkalua. Fishin pyöristystoteutus yksinkertaistaa skriptien kirjoittamista pitämällä matematiikan kuoren ympäristössä sen sijaan, että se kutsuisi muita työkaluja tai kieliä.

## Katso Myös
- Fishin dokumentaatio `math`-komennosta: https://fishshell.com/docs/current/cmds/math.html
- IEEE-standardi liukulukuaritmetiikalle (IEEE 754): https://ieeexplore.ieee.org/document/4610935
