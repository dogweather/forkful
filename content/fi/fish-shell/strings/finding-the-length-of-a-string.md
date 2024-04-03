---
date: 2024-01-20 17:47:19.636867-07:00
description: "How to (Kuinka tehd\xE4) ."
lastmod: '2024-03-13T22:44:56.983166-06:00'
model: gpt-4-1106-preview
summary: .
title: "Merkkijonon pituuden selvitt\xE4minen"
weight: 7
---

## How to (Kuinka tehdä)
```Fish Shell
# Määritä jono
set string "moikka"

# Laske jonon pituus
set length (string length $string)

# Tulosta pituus
echo $length
```
Esimerkin tulostus:
```
6
```

## Deep Dive (Syväsukellus)
Fish shellin `string`-komennon `length`-toiminto julkaistiin Fish 2.3.0:ssa, ja se on säilynyt yhtenä perustyökaluista merkkijonojen käsittelyssä. Se syrjäytti tarpeen käyrätorttuisille Awk- ja Sed-ratkaisuille stringin pituuden mittaamiseen Fishissä. `string length`-komennon toteutus on tehokas ja suoraan ytimessä, verrattuna muihin yleiskäyttöisiin shellicommandoihin, jotka saattavat vaatia pidempiä komentoketjuja saman lopputuloksen aikaansaamiseksi.

## See Also (Katso Myös)
- Fish Shellin virallinen dokumentaatio `string`-komennosta: [link](https://fishshell.com/docs/current/cmds/string.html)
- Fish Shellin GitHub-repositorio: [link](https://github.com/fish-shell/fish-shell)
