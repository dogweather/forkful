---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:16.544687-07:00
description: "Hur g\xF6r man: Fish Shell anv\xE4nder kommandot `test` f\xF6r att kontrollera\
  \ filtyper och egenskaper, inklusive om ett m\xE5l \xE4r en katalog. H\xE4r \xE4\
  r ett\u2026"
lastmod: '2024-03-13T22:44:38.352695-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell anv\xE4nder kommandot `test` f\xF6r att kontrollera filtyper\
  \ och egenskaper, inklusive om ett m\xE5l \xE4r en katalog."
title: Kontrollera om en katalog existerar
weight: 20
---

## Hur gör man:
Fish Shell använder kommandot `test` för att kontrollera filtyper och egenskaper, inklusive om ett mål är en katalog. Här är ett grundläggande mönster för att kontrollera om en katalog finns:

```fish
if test -d /path/to/dir
    echo "Katalogen finns"
else
    echo "Katalogen finns inte"
end
```
Exempelutdata:
```
Katalogen finns
```

För mer strömlinjeformade fil- och katalogoperationer kan man vända sig till externa verktyg som `fd`, även om det oftare används för att hitta filer och kataloger snarare än bara att kontrollera existens. Men genom att kombinera det med Fish-skriptning kan man uppnå praktiska resultat:

```fish
set dir "/path/to/search"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "Katalogen finns"
else
    echo "Katalogen finns inte"
end
```

Detta `fd`-exempel söker efter katalogen på ett angivet djup, och `grep` kontrollerar matchningen, vilket gör det mångsidigt för nyanserade kontroller. Dock, för det direkta syftet att kontrollera existens, är det mer effektivt och okomplicerat att hålla sig till Fish:s inbyggda `test`.
