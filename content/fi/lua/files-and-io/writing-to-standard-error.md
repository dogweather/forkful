---
title:                "Kirjoittaminen standardivirheeseen"
aliases: - /fi/lua/writing-to-standard-error.md
date:                  2024-02-03T19:33:59.300059-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kirjoittaminen standardivirheeseen"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja Miksi?
Kirjoittaminen standardivirheeseen (stderr) tarkoittaa virheviestien ja diagnostiikkatulosteiden ohjaamista erilliseen kanavaan, eroon standarditulosteesta (stdout). Ohjelmoijat tekevät näin erottaakseen tavalliset ohjelman tulokset virhetiedoista, mikä tekee vianetsinnästä ja lokien kirjaamisesta sujuvampaa.

## Miten:
Luassa virheiden kirjoittaminen stderr:iin on mahdollista `io.stderr:write()`-funktion avulla. Tässä on miten voit kirjoittaa yksinkertaisen virheviestin standardivirheeseen:

```lua
io.stderr:write("Virhe: Virheellinen syöte.\n")
```

Jos tarvitset tulostaa muuttujan tai yhdistää useita datan kappaleita, yhdistä ne kirjoitusfunktion sisällä:

```lua
local virheViesti = "Virheellinen syöte."
io.stderr:write("Virhe: " .. virheViesti .. "\n")
```

**Esimerkkituloste stderr:ssä:**
```
Virhe: Virheellinen syöte.
```

Monimutkaisemmissa tilanteissa, tai työskenneltäessä suurempien sovellusten kanssa, saattaa olla aiheellista harkita kolmannen osapuolen lokituskirjastoja, kuten LuaLogging. LuaLoggingin avulla voit ohjata lokit eri kohteisiin, mukaan lukien stderr. Tässä on lyhyt esimerkki:

Ensinnäkin, varmista että LuaLogging on asennettu käyttäen LuaRocks:

```
luarocks install lualogging
```

Sitten, kirjoittaaksesi virheviestin stderr:iin käyttäen LuaLoggingia:

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Virhe: Virheellinen syöte.")
```

Tämä lähestymistapa tarjoaa etuna standardoidun lokituksen sovelluksessasi, lisäten joustavuutta asettaessasi lokitasoja (esim. ERROR, WARN, INFO) yksinkertaisen API:n kautta.
