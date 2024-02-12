---
title:                "Kontrollera om en katalog existerar"
aliases: - /sv/fish-shell/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:16.544687-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kontrollera om en katalog existerar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns i Fish Shell gör att skript kan ta beslut baserade på närvaron eller frånvaron av katalogstrukturer, vilket möjliggör uppgifter som villkorliga filoperationer, loggning eller miljökonfigurering. Denna teknik är avgörande för att skriva robusta skript som interagerar med filsystemet på ett förutsägbart sätt.

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
